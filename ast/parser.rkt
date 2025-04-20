#lang racket

(require "lexer.rkt")
(require parser-tools/lex parser-tools/yacc)

(define calc-parser
  (parser
    (debug "parser-debug.txt")
    (start Expression)
    (end EOF)
    (error void)
    (tokens LITERALS OPERATORS PARENTHESES EOF)
    (grammar
      (PROGRAM
        (DECLARATION-LIST))
      
      (DECLARATION-LIST
        (DECLARATION-LIST DECLARATION)
        (DECLARATION))
      
      (DECLARATION
        (VAR-DECLARATION)
        (FUN-DECLARATION))
      
      (VAR-DECLARATION
        (TYPE-SPECIFIER ID SEMICOLON)
        (TYPE-SPECIFIER ID LSQBRACK EXPRESSION RSQBRACK SEMICOLON))
      
      (TYPE-SPECIFIER
        (INT)
        (VOID)
        (STRING)
        (DOUBLE)
        (AUTO)
        (CHAR))
      
      (FUN-DECLARATION
        (TYPE-SPECIFIER ID LPAR PARAMS RPAR COMPOUND-STMT))
      
      (PARAMS
        (PARAM-LIST)
        (VOID))
      
      (PARAM-LIST
        (PARAM-LIST COMMA PARAM)
        (PARAM))
      
      (PARAM
        (TYPE-SPECIFIER ID)
        (TYPE-SPECIFIER ID LSQBRACK LSQBRACK))
      
      (COMPOUND-STMT
        (LBRACE LOCAL-DECLARATIONS STATEMENT-LIST LBRACE))
      
      (LOCAL-DECLARATIONS
        (LOCAL-DECLARATIONS VAR-DECLARATIONS)
        ())
      
      (STATEMENT-LIST
        (STATEMENT-LIST STATEMENT)
        ())
      
      (STATEMENT
        (EXPRESSION-STMT)
        (COMPOUND-STMT)
        (SELECTION-STMT)
        (ITERATION-STMT)
        (RETURN-STMT))
      
      (EXPRESSION-STMT
        (EXPRESSION SEMICOLON)
        (SEMICOLON))
      
      (SELECTION-STMT
        (IF LPAR EXPRESSION RPAR STATEMENT)
        (IF LPAR EXPRESSION RPAR STATEMENT ELSE STATEMENT))
      
      (ITERATION-STMT
        (WHILE LPAR EXPRESSION RPAR STATEMENT))
      
      (RETURN-STMT
        (RETURN SEMICOLON)
        (RETURN EXPRESSION SEMICOLON))
      
      (EXPRESSION
        (VAR ASSIGN EXPRESSION)
        (SIMPLE-EXPRESSION))
      
      (VAR
        (ID)
        (ID LSQBRACK EXPRESSION RSQBRACK))
      
      (SIMPLE-EXPRESSION
        (ADDITIVE-EXPRESSION RELOP ADDITIVE-EXPRESSION)
        (ADDITIVE-EXPRESSION))
      
      (RELOP
        (LEQ)
        (LT)
        (GT)
        (GEQ)
        (EQ)
        (NEQ))
      
      (ADDITIVE-EXPRESSION
        (ADDITIVE-EXPRESSION ADDOP TERM)
        (TERM))
      
      (ADDOP
        (ADD)
        (SUB))
      
      (TERM
        (TERM MULOP FACTOR)
        (FACTOR))
      
      (MULOP
        (MUL)
        (DIV))
      
      (FACTOR
        (LPAR EXPRESSION RPAR)
        (VAR)
        (CALL)
        (NUM))
      
      (CALL
        (ID LPAR ARGS RPAR))
      
      (ARGS
        (ARG-LIST)
        ())
      
      (ARG-LIST
        (ARG-LIST COMMA EXPRESSION)
        (EXPRESSION))
      )
    (precs
      (left ADD SUBTRACT)
      (left MULTIPLY DIVIDE))))

(provide (all-defined-out))
