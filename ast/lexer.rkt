#lang racket

(require parser-tools/lex (prefix-in : parser-tools/lex-sre))

(define-tokens LITERALS (NUMBER ID LSTRING))

(define-empty-tokens KEYWORDS (IF ELSE RETURN WHILE))
(define-empty-tokens TYPES (INT DOUBLE CHAR STRING VOID AUTO))
(define-empty-tokens RELOPS (LEQ LT GT GEQ EQ NEQ))
(define-empty-tokens LOGICOPS (LAND LOR LNOT))
(define-empty-tokens COLS (SEMICOLON COMMA))
(define-empty-tokens ARITHOPS (ADD SUB MUL DIV))
(define-empty-tokens BRACKETS (LPAR RPAR LBRACE RBRACE LSQBRACK RSQBRACK))
(define-empty-tokens OPS (ASSIGN))
(define-empty-tokens EOF (EOF))

(define cminus-lexer
  (lexer
    ; Keywords
    ("if" (token-IF))
    ("else" (token-ELSE))
    ("return" (token-RETURN))
    ("while" (token-WHILE))

    ; Types
    ("int" (token-INT))
    ("double" (token-DOUBLE))
    ("char" (token-CHAR))
    ("string" (token-STRING))
    ("void" (token-VOID))
    ("auto" (token-AUTO))

    ; Ops
    ("=" (token-ASSIGN))

    ; Relative ops
    ("<=" (token-LEQ))
    ("<" (token-LT))
    (">" (token-GT))
    (">=" (token-GEQ))
    ("==" (token-EQ))
    ("!=" (token-NEQ))

    ; Logical ops
    ("&&" (token-LAND))
    ("||" (token-LOR))
    ("!" (token-LNOT))

    ; Colons
    (";" (token-SEMICOLON))
    ("," (token-COMMA))

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
    (
      (:: #\" (:* (:or "_" (char-range "a" "z") (char-range "A" "Z") (char-range #\0 #\9))) #\")
      (token-LSTRING (substring lexeme 1 (- (string-length lexeme) 1)))
    )

    (whitespace (cminus-lexer input-port))
    ((eof) (token-EOF))))

(define (lex-this prog-string)
  (let ([l (open-input-string prog-string)])
    (lambda () (cminus-lexer l))))

(provide (all-defined-out))

(define (lex-all str)
  (define bib-lex (lex-this str))
  (define (lex-rec tokens)
    (let ([tok (bib-lex)])
      (if (eq? (token-name tok) 'EOF)
          (reverse tokens)
          (lex-rec (cons tok tokens)))))
  (lex-rec '()))
