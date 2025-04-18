#lang racket
(require "ast/parser.rkt")
(require "ast/lexer.rkt")

(displayln (calc-parser (lex-this "1 + 2 * 3")))
