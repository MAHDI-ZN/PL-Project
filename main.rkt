#lang racket
(require "ast/parser.rkt")
(require "ast/lexer.rkt")

(parse-file "tests/21.c")
;# (displayln (calc-parser (lex-this "1 + 2 * 3")))
