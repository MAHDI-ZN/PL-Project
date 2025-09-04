#lang racket

(require "lexer.rkt")
(require "parser.rkt")
(require parser-tools/lex parser-tools/yacc)

;; =============================================================================
;; DATA STRUCTURES (for runtime values, not AST)
;; =============================================================================

;; Values
(struct array-value (elements type) #:transparent)
(struct function-value (params body closure) #:transparent)

;; Environment for variable and function storage
(struct environment (bindings parent) #:transparent #:mutable)

;; =============================================================================
;; ENVIRONMENT MANAGEMENT
;; =============================================================================

(define (make-env [parent #f])
  (environment (make-hash) parent))

(define (env-lookup env name)
  (cond
    [(hash-has-key? (environment-bindings env) name)
     (hash-ref (environment-bindings env) name)]
    [(environment-parent env)
     (env-lookup (environment-parent env) name)]
    [else
     (error 'runtime-error "Undefined variable: ~a" name)]))

;; ---- Type-aware define & set ----

;; A unique sentinel for uninitialized 'auto' variables
(define UNINIT (gensym 'uninit))

;; (value . type)
(define (env-define! env name value type)
  (hash-set! (environment-bindings env) name (cons value type)))

;; Strict, with 'auto' adoption-on-first-assignment
(define (env-set! env name value)
  (cond
    [(hash-has-key? (environment-bindings env) name)
     (let* ([old-binding (hash-ref (environment-bindings env) name)]
            [old-val (car old-binding)]
            [old-ty  (cdr old-binding)]
            [new-ty  (infer-type value)])
       (cond
         [(eq? old-ty 'auto)
          ;; If first time (UNINIT), adopt type; otherwise enforce same type
          (if (eq? old-val UNINIT)
              (hash-set! (environment-bindings env) name (cons value new-ty))
              (begin
                (check-type old-ty new-ty (format "assignment to ~a" name))
                (hash-set! (environment-bindings env) name (cons value old-ty))))]
         [else
          (check-type old-ty new-ty (format "assignment to ~a" name))
          (hash-set! (environment-bindings env) name (cons value old-ty))]))]
    [(environment-parent env)
     (env-set! (environment-parent env) name value)]
    [else
     (error 'runtime-error "Undefined variable: ~a" name)]))

;; =============================================================================
;; TYPE CHECKING
;; =============================================================================

(define (type-compatible? expected actual)
  ;; strict, except 'auto' acts as a type-variable during adoption only
  (eq? expected actual))

(define (infer-type value)
  (cond
    [(eq? value UNINIT) 'auto] ; internal sentinel (only for decl state)
    [(integer? value) 'int]
    [(real? value) 'double]
    [(string? value) 'string]
    [(char? value) 'char]
    [(boolean? value) 'int] ; C-style: booleans behave as ints at runtime
    [(array-value? value) (array-value-type value)]
    [else 'void]))

(define (check-type expected actual context)
  (unless (type-compatible? expected actual)
    (error 'type-error "TypeError: Expected ~a but got ~a in ~a" expected actual context)))

;; =============================================================================
;; INTERPRETER CORE
;; =============================================================================

;; Exception for control flow
(struct return-exception (value) #:transparent)

(define (eval-program prog)
  (let ([global-env (make-env)])
    ;; Add built-in functions
    (add-builtins! global-env)
    ;; Process declarations
    (for-each (lambda (decl) (eval-declaration decl global-env))
              (program-declarations prog))
    ;; Look for main function and call it
    (let ([main-func (env-lookup global-env 'main)])
      (if main-func
          (call-function (car main-func) '() global-env)
          (error 'runtime-error "No main function found")))))

(define (add-builtins! env)
  ;; Add print function
  (env-define! env 'print 
               (lambda (args env)
                 (for-each (lambda (arg) 
                            (display (eval-expression arg env))
                            (display " "))
                          args)
                 (newline)
                 0)
               'void)
  (env-define! env 'parseint 
               (lambda (args env)
                 (unless (= (length args) 1)
                   (error 'runtime-error "parseint expects exactly one argument"))
                 (let* ([arg (eval-expression (car args) env)]
                        [num (string->number arg)])
                   (unless (and num (integer? num))
                     (error 'runtime-error "parseint: string does not represent an integer"))
                   (exact-truncate num)))
               'int))


(define (eval-declaration decl env)
  (cond
    [(var-decl? decl)
     (let ([type (var-decl-type decl)]
           [id (var-decl-id decl)]
           [size (var-decl-size decl)])
       (if size
           ;; Array declaration
           (let* ([size-val (eval-expression size env)]
                  [arr (array-value (make-vector size-val 0) type)])
             ;; If array type is 'auto', keep it as 'auto' until first element assignment.
             (env-define! env id arr type))
           ;; Simple variable declaration
           (env-define! env id (default-value type) type)))]
    
    [(fun-decl? decl)
     (let ([type (fun-decl-type decl)]
           [id (fun-decl-id decl)]
           [params (fun-decl-params decl)]
           [body (fun-decl-body decl)])
       (env-define! env (string->symbol id) (function-value params body env) type))]))

(define (default-value type)
  (case type
    [(auto) UNINIT]          ; important: 'auto' starts uninitialized
    [(int) 0]
    [(double) 0.0]
    [(string) ""]
    [(char) #\null]
    [else 0]))

(define (eval-statement stmt env)
  (cond
    [(expr-stmt? stmt)
     (when (expr-stmt-expr stmt)
       (eval-expression (expr-stmt-expr stmt) env))]
    
    [(compound-stmt? stmt)
     (let ([local-env (make-env env)])
       ;; Process local declarations
       (for-each (lambda (decl) (eval-declaration decl local-env)) 
                 (compound-stmt-local-decls stmt))
       ;; Execute statements
       (for-each (lambda (s) (eval-statement s local-env))
                 (compound-stmt-statements stmt)))]
    
    [(if-stmt? stmt)
     (let ([cond-val (eval-expression (if-stmt-condition stmt) env)])
       (if (truthy? cond-val)
           (eval-statement (if-stmt-then-stmt stmt) env)
           (when (if-stmt-else-stmt stmt)
             (eval-statement (if-stmt-else-stmt stmt) env))))]
    
    [(while-stmt? stmt)
     (let loop ()
       (let ([cond-val (eval-expression (while-stmt-condition stmt) env)])
         (when (truthy? cond-val)
           (eval-statement (while-stmt-body stmt) env)
           (loop))))]
    
    [(return-stmt? stmt)
     (let ([val (if (return-stmt-expr stmt)
                    (eval-expression (return-stmt-expr stmt) env)
                    0)])
       (raise (return-exception val)))]))

;; ---- Lazy evaluation for &&, ||, and now * ----
(define (eval-expression expr env)
  (cond
    [(literal? expr)
     (literal-value expr)]
    
    [(var-expr? expr)
     (let ([binding (env-lookup env (var-expr-id expr))])
       (let ([value (car binding)])
         (if (var-expr-index expr)
             ;; Array access
             (let ([index (eval-expression (var-expr-index expr) env)])
               (unless (array-value? value)
                 (error 'type-error "Cannot index non-array variable"))
               (unless (and (integer? index) (>= index 0) (< index (vector-length (array-value-elements value))))
                 (error 'runtime-error "Array index out of bounds"))
               (vector-ref (array-value-elements value) index))
             ;; Simple variable access
             (if (eq? value UNINIT)
                 (error 'runtime-error "Use of uninitialized variable: ~a" (var-expr-id expr))
                 value))))]
    
    [(assign-expr? expr)
     (let* ([rval (eval-expression (assign-expr-expr expr) env)]
            [var (assign-expr-var expr)])
       (if (var-expr-index var)
           ;; Array assignment
           (let* ([binding (env-lookup env (var-expr-id var))]
                  [array (car binding)]
                  [index (eval-expression (var-expr-index var) env)]
                  [elem-ty (infer-type rval)])
             (unless (array-value? array)
               (error 'type-error "Cannot index non-array variable"))
             (unless (and (integer? index) (>= index 0) (< index (vector-length (array-value-elements array))))
               (error 'runtime-error "Array index out of bounds"))
             ;; If array declared 'auto', adopt type on first element assignment.
             (cond
               [(eq? (array-value-type array) 'auto)
                (define new-arr (array-value (array-value-elements array) elem-ty))
                ;; Update array type in env, then write element
                (env-set! env (var-expr-id var) new-arr)
                (vector-set! (array-value-elements new-arr) index rval)]
               [else
                (check-type (array-value-type array) elem-ty "array element assignment")
                (vector-set! (array-value-elements array) index rval)])
             rval)
           ;; Simple assignment
           (begin
             (env-set! env (var-expr-id var) rval)
             rval)))]
    
    [(binary-expr? expr)
     (define op (binary-expr-op expr))
     (cond
       ;; Lazy && : if left is falsy, don't eval right
       [(eq? op '&&)
        (let ([lv (eval-expression (binary-expr-left expr) env)])
          (if (truthy? lv)
              (let ([rv (eval-expression (binary-expr-right expr) env)])
                (if (and (truthy? lv) (truthy? rv)) 1 0))
              0))]
       ;; Lazy || : if left is truthy, don't eval right
       [(eq? op '||)
        (let ([lv (eval-expression (binary-expr-left expr) env)])
          (if (truthy? lv)
              1
              (let ([rv (eval-expression (binary-expr-right expr) env)])
                (if (or (truthy? lv) (truthy? rv)) 1 0))))]
       ;; NEW: Lazy * : if left is 0, don't eval right
       [(eq? op '*)
        (let ([lv (eval-expression (binary-expr-left expr) env)])
          (if (and (number? lv) (zero? lv))
              0
              (let ([rv (eval-expression (binary-expr-right expr) env)])
                (* lv rv))))]
       [else
        ;; Eager for all other binary ops (numeric & relational)
        (eval-binary-op op 
                        (eval-expression (binary-expr-left expr) env)
                        (eval-expression (binary-expr-right expr) env))])]
    
    [(unary-expr? expr)
     (eval-unary-op (unary-expr-op expr)
                    (eval-expression (unary-expr-expr expr) env))]
    
    [(call-expr? expr)
     (let ([func-binding (env-lookup env (string->symbol (call-expr-id expr)))])
       (let ([func (car func-binding)]
             [args (call-expr-args expr)])
         (call-function func args env)))]))

(define (eval-binary-op op left right)
  (case op
    [(+) (+ left right)]
    [(-) (- left right)]
    ;; (*) handled lazily above; this path used for other ops
    [(/) (if (= right 0)
             (error 'runtime-error "Division by zero")
             (/ left right))]
    [(<) (if (< left right) 1 0)]
    [(>) (if (> left right) 1 0)]
    [(<=) (if (<= left right) 1 0)]
    [(>=) (if (>= left right) 1 0)]
    [(==) (if (equal? left right) 1 0)]
    [(!=) (if (not (equal? left right)) 1 0)]
    [else (error 'runtime-error "Unknown binary operator: ~a" op)]))

(define (eval-unary-op op operand)
  (case op
    [(!) (if (truthy? operand) 0 1)]
    [(-) (- operand)]
    [else (error 'runtime-error "Unknown unary operator: ~a" op)]))

(define (truthy? val)
  (not (or (eq? val 0) (eq? val #f))))

;; ---- Function calls enforce/learn types for params ----
(define (call-function func args env)
  (cond
    [(procedure? func)
     ;; Built-in function
     (func args env)]
    
    [(function-value? func)
     ;; User-defined function
     (let* ([params (function-value-params func)]
            [body (function-value-body func)]
            [closure (function-value-closure func)]
            [func-env (make-env closure)])
       
       ;; Check arity
       (unless (= (length params) (length args))
         (error 'runtime-error "Function arity mismatch: expected ~a args, got ~a" 
                (length params) (length args)))
       
       ;; Bind parameters with strict typing / auto-adoption
       (for-each
        (lambda (param arg)
          (let* ([pv (param-type param)]
                 [arg-val (eval-expression arg env)]
                 [arg-ty (infer-type arg-val)])
            (cond
              [(eq? pv 'auto)
               (env-define! func-env (param-id param) arg-val arg-ty)]
              [else
               (check-type pv arg-ty (format "argument for parameter ~a" (param-id param)))
               (env-define! func-env (param-id param) arg-val pv)])))
        params args)
       
       ;; Execute function body
       (with-handlers ([return-exception? (lambda (e) (return-exception-value e))])
         (eval-statement body func-env)
         0))]  ; Default return value
    
    [else
     (error 'runtime-error "Cannot call non-function value")]))

;; =============================================================================
;; MAIN INTERFACE
;; =============================================================================

(define (interpret-file path)
  (let ([ast (parse-file path)])  ; This calls the parser
    (eval-program ast)))

(define (interpret-string code)
  (let ([ast (cminus-parser (lex-this code))])  ; This calls the parser
    (eval-program ast)))

(provide (all-defined-out))
