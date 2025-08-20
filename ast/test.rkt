#lang racket

(require "lexer.rkt")
(require parser-tools/lex parser-tools/yacc)

(define cminus-parser
  (parser
    (debug "parser-debug.txt")
    (start PROGRAM)
    (end EOF)
    (error void)
    (tokens LITERALS KEYWORDS TYPES RELOPS COLS ARITHOPS BRACKETS OPS EOF LOGICOPS)
    (grammar
;       (DUMMY
;         ;((VAR-DECLARATION) (void))
;         ((STATEMENT) (void)))
;         ;((EXPRESSION) (void)))
      (PROGRAM
        ((DECLARATION-LIST) (void)))
      (DECLARATION-LIST
        ((DECLARATION-LIST DECLARATION) (void))
        ((DECLARATION) (void)))
      (DECLARATION
        ((VAR-DECLARATION) (void))
        ((FUN-DECLARATION) (void)))
      (VAR-DECLARATION
        ((TYPE-SPECIFIER ID SEMICOLON) (void))
        ((TYPE-SPECIFIER ID LSQBRACK EXPRESSION RSQBRACK SEMICOLON) (void)))
      (TYPE-SPECIFIER
        ((INT) (void))
        ((VOID) (void))
        ((STRING) (void))
        ((DOUBLE) (void))
        ((AUTO) (void))
        ((CHAR) (void)))
      (FUN-DECLARATION
        ((TYPE-SPECIFIER ID LPAR PARAMS RPAR COMPOUND-STMT) (void)))
      (PARAMS
        ((PARAM-LIST) (void))
        ((VOID) (void)))
      (PARAM-LIST
        ((PARAM-LIST COMMA PARAM) (void))
        ((PARAM) (void)))
      (PARAM
        ((TYPE-SPECIFIER ID) (void))
        ((TYPE-SPECIFIER ID LSQBRACK RSQBRACK) (void)))
      (COMPOUND-STMT
        ((LBRACE LOCAL-DECLARATIONS STATEMENT-LIST RBRACE) (void)))
      (LOCAL-DECLARATIONS
        ((LOCAL-DECLARATIONS VAR-DECLARATION) (void))
        (() (void)))
      (STATEMENT-LIST
        ((STATEMENT-LIST STATEMENT) (void))
        (() (void)))
      (STATEMENT
        ((EXPRESSION-STMT) (void))
        ((COMPOUND-STMT) (void))
        ((SELECTION-STMT) (void))
        ((ITERATION-STMT) (void))
        ((RETURN-STMT) (void)))
      (EXPRESSION-STMT
        ((EXPRESSION SEMICOLON) (void))
        ((SEMICOLON) (void)))
      (SELECTION-STMT
        ((IF LPAR EXPRESSION RPAR STATEMENT) (void))
        ((IF LPAR EXPRESSION RPAR STATEMENT ELSE STATEMENT) (void)))
      (ITERATION-STMT
        ((WHILE LPAR EXPRESSION RPAR STATEMENT) (void)))
      (RETURN-STMT
        ((RETURN SEMICOLON) (void))
        ((RETURN EXPRESSION SEMICOLON) (void)))
      (EXPRESSION
        ((VAR ASSIGN EXPRESSION) (void))
        ((SIMPLE-EXPRESSION) (void)))
      (VAR
        ((ID) (void))
        ((ID LSQBRACK EXPRESSION RSQBRACK) (void)))
      (SIMPLE-EXPRESSION
        ((ADDITIVE-EXPRESSION RELOP ADDITIVE-EXPRESSION) (void))
        ((ADDITIVE-EXPRESSION) (void))
        ((LSTRING) (void)))
      (RELOP
        ((LEQ) (void))
        ((LT) (void))
        ((GT) (void))
        ((GEQ) (void))
        ((EQ) (void))
        ((NEQ) (void)))
      (ADDITIVE-EXPRESSION
        ((ADDITIVE-EXPRESSION ADDOP TERM) (void))
        ((TERM) (void)))
      (ADDOP
        ((ADD) (void))
        ((SUB) (void)))
      (TERM
        ((TERM MULOP NEW-FACTOR) (void))
        ((NEW-FACTOR) (void)))
      (MULOP
        ((MUL) (void))
        ((DIV) (void)))
      (NEW-FACTOR
        ((NEW-FACTOR DUOLOGOP FACTOR) (void))
        ((LNOT FACTOR) (void))
        ((FACTOR) (void)))
      (DUOLOGOP
        ((LAND) (void))
        ((LOR) (void)))
      (FACTOR
        ((LPAR EXPRESSION RPAR) (void))
        ((VAR) (void))
        ((CALL) (void))
        ((NUMBER) (void)))
      (CALL
        ((ID LPAR ARGS RPAR) (void)))
      (ARGS
        ((ARG-LIST) (void))
        (() (void)))
      (ARG-LIST
        ((ARG-LIST COMMA EXPRESSION) (void))
        ((EXPRESSION) (void)))
)))

(define (parse-file path)
  (cminus-parser (lex-this (string-join (file->lines path)))))

(define test-program "a.c")
(define ast (parse-file test-program))
(display ast)

(provide (all-defined-out))