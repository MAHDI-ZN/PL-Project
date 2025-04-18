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
      (Expression
        ((NUMBER) $1)
        ((LPAREN Expression RPAREN) $2)
        ((Expression ADD Expression) (+ $1 $3))
        ((Expression SUBTRACT Expression) (- $1 $3))
        ((Expression MULTIPLY Expression) (* $1 $3))
        ((Expression DIVIDE Expression) (quotient $1 $3))))
    (precs
      (left ADD SUBTRACT)
      (left MULTIPLY DIVIDE))))

(provide (all-defined-out))
