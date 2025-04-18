#lang racket

(require parser-tools/lex (prefix-in : parser-tools/lex-sre))

(define-tokens LITERALS (NUMBER))
(define-empty-tokens OPERATORS (ADD SUBTRACT MULTIPLY DIVIDE))
(define-empty-tokens PARENTHESES (LPAREN RPAREN))
(define-empty-tokens EOF (EOF))

(define calc-lexer
  (lexer
    ("+" (token-ADD))
    ("-" (token-SUBTRACT))
    ("*" (token-MULTIPLY))
    ("/" (token-DIVIDE))
    ("(" (token-LPAREN))
    (")" (token-RPAREN))
    ((:+ numeric) (token-NUMBER (string->number lexeme)))
    (whitespace (calc-lexer input-port))
    ((eof) (token-EOF))))

(define (lex-this prog-string)
  (let ([l (open-input-string prog-string)])
    (begin
      (lambda ()
        (calc-lexer l)
        ))))

(provide (all-defined-out))
