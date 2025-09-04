#lang racket
;(require "ast/lexer.rkt")
;(require "ast/parser.rkt")
(require "ast/interpreter.rkt")
(interpret-file "phase2/4.c")
;(interpret-string "int main(void) { print(42); return 0; }")
;(parse-file "tests/08.c")
;# (displayln (calc-parser (lex-this "1 + 2 * 3")))
