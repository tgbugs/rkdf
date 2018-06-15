#lang racket/base
(require brag/support
         (rename-in br-parser-tools/lex-sre (- :-)))
(provide turtle-make-tokenizer)

(define-lex-abbrev digits (:+ (char-set "0123456789")))
(define-lex-abbrev newlines (:+ (char-set "\xA\xD")))
;(define-lex-abbrev char (char-set " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~"))
;(define-lex-abbrev char (char-set "!#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[]^_`abcdefghijklmnopqrstuvwxyz{|}~")) ; \\ and "
;(define-lex-abbrev chars (:+ (char-set "!#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[]^_`abcdefghijklmnopqrstuvwxyz{|}~"))) ; \\ and "
;(define-lex-abbrev char-safe (char-set "!#$%&'()*+,-./0123456789:;<=?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[]^_`abcdefghijklmnopqrstuvwxyz{|}~")) ; \\ and "
;(define-lex-abbrev chars-safe (char-set "!#$%&'()*+,-./0123456789;=?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[]^_`abcdefghijklmnopqrstuvwxyz{|}~")) ; \\ and "
(define-lex-abbrev extra-chars (char-set "!#$%&'()*+,-./:;<=>?@[]^_`{|}~")) ; \\ and "
(define-lex-abbrev 0-9 (char-set "0123456789"))
(define-lex-abbrev lower-case (char-set "abcdefghijklmnopqrstuvwxyz"))
(define-lex-abbrev upper-case (char-set "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
;["\x00C0"-"\x00D6"] | ["\x00D8"-"\x00F6"] | ["\x00F8"-"\x02FF"] | ["\x0370"-"\x037D"] | ["\x037F"-"\x1FFF"] | ["\x200C"-"\x200D"] | ["\x2070"-"\x218F"] | ["\x2C00"-"\x2FEF"] | ["\x3001"-"\xD7FF"] | ["\xF900"-"\xFDCF"] | ["\xFDF0"-"\xFFFD"] | ["\x10000"-"\xEFFFF"]
(define (turtle-make-tokenizer port)
  (define (next-token)
    (define rdf-lexer
      (lexer-srcloc
       [(eof) (return-without-srcloc eof)]
       ;[digits (token 'NUMBER (string->number lexeme))]  ; TODO octal and hex
       ["." (token 'PERIOD lexeme)]
       ["@prefix" (token 'PREFIX)]
       ["@base" (token 'BASE)]
       ["@" (token 'AMP lexeme)]
       ["a" (token 'CHAR-AL lexeme)]
       ["e" (token 'CHAR-EL lexeme)]
       ["E" (token 'CHAR-EU lexeme)]
       [":" (token 'COLON lexeme)]
       [";" (token 'SEMICOLON lexeme)]
       ["," (token 'COMMA lexeme)]
       ;["<" (token 'OB)]
       ;[">" (token 'CB)]
       [(from/to "<" ">") (token 'URIREF (substring lexeme 1 (sub1 (string-length lexeme))))]
       ["[]" (token 'OSBCSB '(bnode))]
       ["[" (token 'OSB lexeme)]
       ["]" (token 'CSB lexeme)]
       ["(" (token 'OP lexeme)]
       [")" (token 'CP lexeme)]
       [" " (token 'SPACE lexeme)]
       ["^^" (token 'DATATYPE lexeme)]
       ;[upper-case (token 'A-Z lexeme)]
       ;[lower-case (token 'a-z lexeme)]
       ;[chars (token 'CHARS lexeme)]
       ["\n" (token 'NEWLINE lexeme)]
       ["\r" (token 'CR lexeme)]  ; FIXME normalize to \n?
       ;(define-lex-abbrev symbol-chars (:+ (:or alphabetic symbolic numeric (char-set "-*_&%#!?.:/"))))
       ;[(char-set "abcdefghijklmnopqrstuvwxyz") (token 'LOWER)]
       ;[(char-set "ABCDEFGHIJKLMNOPQRSTUVWXYZ") (token 'UPPER)]
       [(from/to "\"\"\"" "\"\"\"") (token 'LONGSTRING (substring lexeme 3 (- (string-length lexeme) 3)))]
       [(from/to "\"" "\"") (token 'STRING (substring lexeme 1 (sub1 (string-length lexeme))))]  ; FIXME escape sequences?
       ;["\"" (token 'DQUOTE)]
       ["\t" (token 'TAB)]
       ["\\u" (token 'UNICODE-START-L)]
       ["\\U" (token 'UNICODE-START-U)]
       ["true" (token 'TRUE lexeme)]
       ["false" (token 'FALSE lexeme)]
       ;[char-safe (token 'CHARSAFE lexeme)]
       ;[char (token 'CHAR lexeme)]
       ;[(:+ lower-case upper-case char-safe) (token 'CHARSSAFE lexeme)]
       [lower-case (token 'LOWER lexeme)]
       [(:or upper-case "_") (token 'NSCHAR lexeme)]
       ;[(:or lower-case upper-case char-safe) (token 'CHARSAFE lexeme)]
       [extra-chars (token 'CHARSAFE lexeme)]
       [(:or extra-chars ">") (token 'CHAR lexeme)]
       ;[0-9 (token '0-9 (string->number lexeme))]  ; TODO octal and hex
       [0-9 (token '0-9 lexeme)]  ; TODO octal and hex
       ;[lower-case (token 'LOWER lexeme)]
       ;[upper-case (token 'UPPER lexeme)]
       [(from/to "#" newlines) (token 'COMMENT)]  ; FIXME this is superseding char for some reason?
       ))
    (rdf-lexer port))
  next-token)

