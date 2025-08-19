#lang racket

(require "lexer.rkt")
(require "parser.rkt")
(require parser-tools/lex parser-tools/yacc)

;; =============================================================================
;; DATA STRUCTURES
;; =============================================================================

;; AST Node Structures
(struct program (declarations) #:transparent)
(struct var-decl (type id size) #:transparent) ; size is #f for simple vars, expr for arrays
(struct fun-decl (type id params body) #:transparent)
(struct param (type id is-array?) #:transparent)

;; Statements
(struct compound-stmt (local-decls statements) #:transparent)
(struct expr-stmt (expr) #:transparent)
(struct if-stmt (condition then-stmt else-stmt) #:transparent) ; else-stmt can be #f
(struct while-stmt (condition body) #:transparent)
(struct return-stmt (expr) #:transparent) ; expr can be #f

;; Expressions
(struct assign-expr (var expr) #:transparent)
(struct binary-expr (op left right) #:transparent)
(struct unary-expr (op expr) #:transparent)
(struct call-expr (id args) #:transparent)
(struct var-expr (id index) #:transparent) ; index is #f for simple vars
(struct literal (value type) #:transparent)

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

(define (env-define! env name value type)
  (hash-set! (environment-bindings env) name (cons value type)))

(define (env-set! env name value)
  (cond
    [(hash-has-key? (environment-bindings env) name)
     (let ([old-binding (hash-ref (environment-bindings env) name)])
       (hash-set! (environment-bindings env) name (cons value (cdr old-binding))))]
    [(environment-parent env)
     (env-set! (environment-parent env) name value)]
    [else
     (error 'runtime-error "Undefined variable: ~a" name)]))

;; =============================================================================
;; TYPE CHECKING
;; =============================================================================

(define (type-compatible? expected actual)
  (or (eq? expected actual)
      (eq? expected 'auto)
      (eq? actual 'auto)))

(define (infer-type value)
  (cond
    [(integer? value) 'int]
    [(real? value) 'double]
    [(string? value) 'string]
    [(char? value) 'char]
    [(boolean? value) 'int] ; C-style: booleans are integers
    [(array-value? value) (array-value-type value)]
    [else 'void]))

(define (check-type expected actual context)
  (unless (type-compatible? expected actual)
    (error 'type-error "TypeError: Expected ~a but got ~a in ~a" expected actual context)))

;; =============================================================================
;; UPDATED PARSER (with AST generation)
;; =============================================================================

(define cminus-parser
  (parser
    (debug "parser-debug.txt")
    (start PROGRAM)
    (end EOF)
    (error void)
    (tokens LITERALS KEYWORDS TYPES RELOPS COLS ARITHOPS BRACKETS OPS EOF LOGICOPS)
    (grammar
      (PROGRAM
        ((DECLARATION-LIST) (program $1)))
      
      (DECLARATION-LIST
        ((DECLARATION-LIST DECLARATION) (append $1 (list $2)))
        ((DECLARATION) (list $1)))
      
      (DECLARATION
        ((VAR-DECLARATION) $1)
        ((FUN-DECLARATION) $1))
      
      (VAR-DECLARATION
        ((TYPE-SPECIFIER ID SEMICOLON) (var-decl $1 $2 #f))
        ((TYPE-SPECIFIER ID LSQBRACK EXPRESSION RSQBRACK SEMICOLON) (var-decl $1 $2 $4)))
      
      (TYPE-SPECIFIER
        ((INT) 'int)
        ((VOID) 'void)
        ((STRING) 'string)
        ((DOUBLE) 'double)
        ((AUTO) 'auto)
        ((CHAR) 'char))
      
      (FUN-DECLARATION
        ((TYPE-SPECIFIER ID LPAR PARAMS RPAR COMPOUND-STMT) (fun-decl $1 $2 $4 $6)))
      
      (PARAMS
        ((PARAM-LIST) $1)
        ((VOID) '()))
      
      (PARAM-LIST
        ((PARAM-LIST COMMA PARAM) (append $1 (list $3)))
        ((PARAM) (list $1)))
      
      (PARAM
        ((TYPE-SPECIFIER ID) (param $1 $2 #f))
        ((TYPE-SPECIFIER ID LSQBRACK RSQBRACK) (param $1 $2 #t)))
      
      (COMPOUND-STMT
        ((LBRACE LOCAL-DECLARATIONS STATEMENT-LIST RBRACE) (compound-stmt $2 $3)))
      
      (LOCAL-DECLARATIONS
        ((LOCAL-DECLARATIONS VAR-DECLARATION) (append $1 (list $2)))
        (() '()))
      
      (STATEMENT-LIST
        ((STATEMENT-LIST STATEMENT) (append $1 (list $2)))
        (() '()))
      
      (STATEMENT
        ((EXPRESSION-STMT) $1)
        ((COMPOUND-STMT) $1)
        ((SELECTION-STMT) $1)
        ((ITERATION-STMT) $1)
        ((RETURN-STMT) $1))
      
      (EXPRESSION-STMT
        ((EXPRESSION SEMICOLON) (expr-stmt $1))
        ((SEMICOLON) (expr-stmt #f)))
      
      (SELECTION-STMT
        ((IF LPAR EXPRESSION RPAR STATEMENT) (if-stmt $3 $5 #f))
        ((IF LPAR EXPRESSION RPAR STATEMENT ELSE STATEMENT) (if-stmt $3 $5 $7)))
      
      (ITERATION-STMT
        ((WHILE LPAR EXPRESSION RPAR STATEMENT) (while-stmt $3 $5)))
      
      (RETURN-STMT
        ((RETURN SEMICOLON) (return-stmt #f))
        ((RETURN EXPRESSION SEMICOLON) (return-stmt $2)))
      
      (EXPRESSION
        ((VAR ASSIGN EXPRESSION) (assign-expr $1 $3))
        ((SIMPLE-EXPRESSION) $1))
      
      (VAR
        ((ID) (var-expr $1 #f))
        ((ID LSQBRACK EXPRESSION RSQBRACK) (var-expr $1 $3)))
      
      (SIMPLE-EXPRESSION
        ((ADDITIVE-EXPRESSION RELOP ADDITIVE-EXPRESSION) (binary-expr $2 $1 $3))
        ((ADDITIVE-EXPRESSION) $1)
        ((LSTRING) (literal $1 'string)))
      
      (RELOP
        ((LEQ) '<=)
        ((LT) '<)
        ((GT) '>)
        ((GEQ) '>=)
        ((EQ) '==)
        ((NEQ) '!=))
      
      (ADDITIVE-EXPRESSION
        ((ADDITIVE-EXPRESSION ADDOP TERM) (binary-expr $2 $1 $3))
        ((TERM) $1))
      
      (ADDOP
        ((ADD) '+)
        ((SUB) '-))
      
      (TERM
        ((TERM MULOP NEW-FACTOR) (binary-expr $2 $1 $3))
        ((NEW-FACTOR) $1))
      
      (MULOP
        ((MUL) '*)
        ((DIV) '/))
      
      (NEW-FACTOR
        ((NEW-FACTOR DUOLOGOP FACTOR) (binary-expr $2 $1 $3))
        ((LNOT FACTOR) (unary-expr '! $2))
        ((FACTOR) $1))
      
      (DUOLOGOP
        ((LAND) '&&)
        ((LOR) '||))
      
      (FACTOR
        ((LPAR EXPRESSION RPAR) $2)
        ((VAR) $1)
        ((CALL) $1)
        ((NUMBER) (literal $1 (if (integer? $1) 'int 'double))))
      
      (CALL
        ((ID LPAR ARGS RPAR) (call-expr $1 $3)))
      
      (ARGS
        ((ARG-LIST) $1)
        (() '()))
      
      (ARG-LIST
        ((ARG-LIST COMMA EXPRESSION) (append $1 (list $3)))
        ((EXPRESSION) (list $1)))
)))

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
    (for-each (lambda (decl) (eval-declaration decl global-env)) (program-declarations prog))
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
               'void))

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
             (env-define! env id arr type))
           ;; Simple variable declaration
           (env-define! env id (default-value type) type)))]
    
    [(fun-decl? decl)
     (let ([type (fun-decl-type decl)]
           [id (fun-decl-id decl)]
           [params (fun-decl-params decl)]
           [body (fun-decl-body decl)])
       (env-define! env id (function-value params body env) type))]))

(define (default-value type)
  (case type
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
             value)))]
    
    [(assign-expr? expr)
     (let* ([rval (eval-expression (assign-expr-expr expr) env)]
            [var (assign-expr-var expr)])
       (if (var-expr-index var)
           ;; Array assignment
           (let* ([binding (env-lookup env (var-expr-id var))]
                  [array (car binding)]
                  [index (eval-expression (var-expr-index var) env)])
             (unless (array-value? array)
               (error 'type-error "Cannot index non-array variable"))
             (unless (and (integer? index) (>= index 0) (< index (vector-length (array-value-elements array))))
               (error 'runtime-error "Array index out of bounds"))
             (vector-set! (array-value-elements array) index rval)
             rval)
           ;; Simple assignment
           (begin
             (env-set! env (var-expr-id var) rval)
             rval)))]
    
    [(binary-expr? expr)
     (eval-binary-op (binary-expr-op expr) 
                     (eval-expression (binary-expr-left expr) env)
                     (eval-expression (binary-expr-right expr) env))]
    
    [(unary-expr? expr)
     (eval-unary-op (unary-expr-op expr)
                    (eval-expression (unary-expr-expr expr) env))]
    
    [(call-expr? expr)
     (let ([func-binding (env-lookup env (call-expr-id expr))])
       (let ([func (car func-binding)]
             [args (call-expr-args expr)])
         (call-function func args env)))]))

(define (eval-binary-op op left right)
  (case op
    [(+) (+ left right)]
    [(-) (- left right)]
    [(*) (* left right)]
    [(/) (if (= right 0)
             (error 'runtime-error "Division by zero")
             (/ left right))]
    [(<) (if (< left right) 1 0)]
    [(>) (if (> left right) 1 0)]
    [(<=) (if (<= left right) 1 0)]
    [(>=) (if (>= left right) 1 0)]
    [(==) (if (equal? left right) 1 0)]
    [(!=) (if (not (equal? left right)) 1 0)]
    [(&&) (if (and (truthy? left) (truthy? right)) 1 0)]
    [(||) (if (or (truthy? left) (truthy? right)) 1 0)]
    [else (error 'runtime-error "Unknown binary operator: ~a" op)]))

(define (eval-unary-op op operand)
  (case op
    [(!) (if (truthy? operand) 0 1)]
    [(-) (- operand)]
    [else (error 'runtime-error "Unknown unary operator: ~a" op)]))

(define (truthy? val)
  (not (or (eq? val 0) (eq? val #f))))

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
       
       ;; Bind parameters
       (for-each (lambda (param arg)
                   (let ([arg-val (eval-expression arg env)])
                     (env-define! func-env (param-id param) arg-val (param-type param))))
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
  (let ([ast (parse-file path)])
    (eval-program ast)))

(define (interpret-string code)
  (let ([ast (cminus-parser (lex-this code))])
    (eval-program ast)))

(provide (all-defined-out))
