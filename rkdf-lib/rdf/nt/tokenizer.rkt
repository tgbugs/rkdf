#lang racket/base
(require brag/support
         (rename-in br-parser-tools/lex-sre (- :-)))
(provide nt-make-tokenizer)

(define-lex-abbrev newlines (:+ (char-set "\xA\xD")))
(define-lex-abbrev 0-9 (char-set "0123456789"))
(define-lex-abbrev lower-case (char-set "abcdefghijklmnopqrstuvwxyz"))

(define (nt-make-tokenizer port)
  (define (next-token)
    (define rdf-lexer
      (lexer;-srcloc
       ;[(eof) (return-without-srcloc eof)]
       [(eof) eof]
       ["." (token 'PERIOD lexeme)]
       [(from/to "<" ">") (token 'URIREF (substring lexeme 1 (sub1 (string-length lexeme))))]
       [(from/to "\"" "\"") (token 'STRING (substring lexeme 1 (sub1 (string-length lexeme))))]  ; FIXME escape sequences?
       [(from/to "_:" whitespace) (token 'NODE (substring lexeme 2 (sub1 (string-length lexeme))))]  ; TODO stop on all whitespace
       [(from/to "#" newlines) (token 'COMMENT)]  ; FIXME this is superseding char for some reason?
       [whitespace (token lexeme #:skip? #t)]
       [(from/to "@" whitespace) (token 'LANG lexeme)]
       ["^^" (token 'DATATYPE lexeme)]
       [lower-case (token 'LOWER lexeme)]
       [0-9 (token '0-9 lexeme)]  ; TODO octal and hex
       ))
    (rdf-lexer port))
  next-token)

