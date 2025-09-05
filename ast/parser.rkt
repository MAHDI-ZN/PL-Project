#lang racket

(require "lexer.rkt")
(require parser-tools/lex parser-tools/yacc)

;; AST Node Structures (add these to your parser file)
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
        ((NUMBER) (literal $1 (cond
                          [(exact-integer? $1) 'int]
                          [(and (real? $1) (not (exact-integer? $1))) 'double]
                          [else 'int])))
        ((SUB FACTOR) (unary-expr '- $2))
        ((ADD FACTOR) $2))
      
      (CALL
        ((ID LPAR ARGS RPAR) (call-expr $1 $3)))
      
      (ARGS
        ((ARG-LIST) $1)
        (() '()))
      
      (ARG-LIST
        ((ARG-LIST COMMA EXPRESSION) (append $1 (list $3)))
        ((EXPRESSION) (list $1)))
)))

(define (parse-file path)
  (cminus-parser (lex-this (string-join (file->lines path)))))

(provide (all-defined-out))
