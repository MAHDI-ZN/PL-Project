#lang racket

(require parser-tools/lex 
         (prefix-in : parser-tools/lex-sre))


(define-tokens LITERALS (NUMBER ID))

(define-empty-tokens KEYWORDS (IF ELSE RETURN WHILE INT DOUBLE CHAR STRING VOID))
(define-empty-tokens RELOPS (LEQ LT GT GEQ EQ NEQ))
(define-empty-tokens ARITHOPS (ADD SUB MUL DIV))
(define-empty-tokens BRACKETS (LPAR RPAR LBRACE RBRACE LSQBRACK RSQBRACK))
(define-empty-tokens OPS (ASSIGN))
(define-empty-tokens EOF (EOF))

(define bython-lexer
  (lexer
    ; Keywords
    ("if" (token-IF))
    ("else" (token-ELSE))
    ("return" (token-RETURN))
    ("while" (token-WHILE))
    ("int" (token-INT))
    ("double" (token-DOUBLE))
    ("char" (token-CHAR))
    ("string" (token-STRING))
    ("void" (token-VOID))

    ; Ops
    ("=" (token-ASSIGN))

    ; Relative ops
    ("<=" (token-LEQ))
    ("<" (token-LT))
    (">" (token-GT))
    (">=" (token-GEQ))
    ("==" (token-EQ))
    ("!=" (token-NEQ))

    ; Arithmetic ops
    ("+" (token-ADD))
    ("-" (token-SUB))
    ("*" (token-MUL))
    ("/" (token-DIV))
  
    ; Brackets
    ("(" (token-LPAR))
    (")" (token-RPAR))
    ("{" (token-LBRACE))
    ("}" (token-RBRACE))
    ("[" (token-LSQBRACK))
    ("]" (token-RSQBRACK))
    ; Literals
    (
      (:or (:+ (char-range #\0 #\9))
           (:: (:+ (char-range #\0 #\9)) #\. (:+ (char-range #\0 #\9))))
      (token-NUMBER (string->number lexeme))
    )
    (
      (:: (:or "_" (char-range "a" "z") (char-range "A" "Z"))
          (:* (:or "_" (char-range "a" "z") (char-range "A" "Z") (char-range #\0 #\9)))
      )
      (token-ID lexeme)
    )

    (whitespace (bython-lexer input-port))
    ((eof) (token-EOF))))

(define (lex-this prog-string)
  (let ([l (open-input-string prog-string)])
    (begin
      (lambda ()
        (bython-lexer l)
        ))))

(provide (all-defined-out))


; tests
; TODO: Remove these
(define bib-lex (lex-this "int main() {x = 10 y = 10 if (x == 10) {return 1}}"))

(define (lex-all)
  (define (lex-rec tokens)
    (let ([tok (bib-lex)])
      (if (eq? (token-name tok) 'EOF)
          (reverse tokens)
          (lex-rec (cons tok tokens)))))
  (lex-rec '()))

(lex-all)
