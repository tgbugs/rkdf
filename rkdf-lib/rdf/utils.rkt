#lang racket/base
(require racket/generic
         racket/list
         racket/hash
         racket/string
         racket/stream
         (only-in racket/dict in-dict)
         (only-in racket/sequence sequence->list)
         (for-syntax racket/base
                     racket/string
                     racket/syntax
                     syntax/parse))
(provide (all-defined-out)
         (struct-out Literal)
         (struct-out triple)
         (struct-out BNode)
         (struct-out URI))
(module+ test
  (require rackunit (for-syntax rackunit)))

(define (bracket uri)
  (string-append "<" uri ">"))

(define-generics serializable
  ; TODO consider using current output port to do this directly
  (gen-ser serializable)
  (gen-value serializable)
  (gen-short serializable)
  )

(define iri-prefixes (make-parameter (make-hash)))

;;; internal representation of entities

(struct URI (value) #:inspector #f
  #:methods gen:serializable
  [(define (gen-value uri) (URI-value uri))
   (define (gen-short uri) (qname (URI-value uri)))
   (define (gen-ser uri)
     (string-append (bracket (URI-value uri))))])

(struct triple (s p o) #:inspector #f
  #:methods gen:serializable
  ; TODO
  [(define (gen-ser trip)
     trip)])

(struct Literal (value type lang) #:inspector #f
  #:methods gen:serializable
  [(define (gen-value lit) (Literal-value lit))
   (define (gen-ser lit)
     (let* ([value (Literal-value lit)]
            [type (Literal-type lit)]
            [lang (Literal-lang lit)]
            [temp-value (cond [(number? value) (number->string value)]
                              [(boolean? value) (if value "true" "false")]
                              [else value])]
            [out-value (string-append "\""
                                      (string-replace temp-value "\n" "\\n")
                                      "\"")])
       (cond
         [(not (null? type))
          (cond ;[(eq? (resource (qname "rdf" "XMLLiteral")) type)
            ; (string-replace)]
            [else (string-append out-value "^^"
                                 (bracket (URI-value type)))])]
         [(not (null? lang)) (string-append out-value "@" lang)]
         [else out-value])))])

(struct BNode (value) #:inspector #f
  #:methods gen:serializable
  [(define (gen-value bnode) (BNode-value bnode))
   (define (gen-ser bnode)
     (string-append "_:" (BNode-value bnode)))])

;;; store

(define (instrument-store [store '()])
  (define int-triples store)

  (define (add-triple triple)
    (set! int-triples (cons triple int-triples)))

  (define (add-triples triples)
    (set! int-triples (append triples int-triples)))

  (define-syntax (null?-or-equal? stx)
    (syntax-parse stx
      [(_ nullable to-match)
       #'(or (null? nullable) (equal? nullable to-match))]))

  (define (-match-triples #:s [s null] #:p [p null] #:o [o null])
    (define (f s-t p-t o-t)
      (and (null?-or-equal? s s-t)
           (null?-or-equal? p p-t)
           (null?-or-equal? o o-t)
           ))
    (filter (λ (t) (apply f t)) int-triples))

  (define (match-triples #:s [s null] #:p [p null] #:o [o null] #:transitive [transitive #f])
    "if #:transitive is set the a value given to #:s or to #:o is interpreted as the starting point"
    (define (f t)
      (and (null?-or-equal? s (gen-value (triple-s t)))
           (null?-or-equal? p (gen-value (triple-p t)))
           (null?-or-equal? o (gen-value (triple-o t)))))
    (if transitive
        ; get the first set of result triples
        ; extra the object if subject and subject if object
        ; go next
        ; this is a horrible algorithem
        ; FIXME can use transitive as the accumulator
        ; FIXME cycles
        (let ([matches (filter f int-triples)])
          (cond [(null? matches) matches]
                [(null? p)
                 (raise-arguments-error 'predicate-null
                                        "#:p cannot be null if #:transitive is set"
                                        "#:p" p
                                        "#:transitive" transitive)]
                [(and (null? s) (null? o))
                 (for/fold ([out matches]
                            #:result out)
                           ([s (map (compose gen-value triple-s) matches)]
                            [o (map (compose gen-value triple-o) matches)])
                   (append (match-triples #:s o #:p p #:transitive transitive)
                           (append (match-triples #:p p #:o s #:transitive transitive)
                                   out)))]
                [(null? s)  ; search 'backward'
                 (for/fold ([out matches]
                            #:result out)
                           ([s (map (compose gen-value triple-s) matches)])
                   (append (match-triples #:p p #:o s #:transitive transitive) out))]
                [(null? o)  ; search 'forward'
                 (for/fold ([out matches]
                            #:result out)
                           ([o (map (compose gen-value triple-o) matches)])
                   (append (match-triples #:s o #:p p #:transitive transitive) out))]
                [else (raise-arguments-error 'help! "No idea how we got here.")]))
        (filter f int-triples)))

  (values add-triple add-triples (λ () int-triples) match-triples))

(module+ test
  (define store
    (list
     (triple (URI "s0") (URI "p0") (URI "o0"))
     (triple (URI "o0") (URI "p0") (URI "o1"))
     ))
  (define-values (add-triple add-triples get-triples match-triples) (instrument-store store))
  (define out-test (for/fold ([out null] #:result out) ([i '(1 2 3)]) (values (cons i out))))
  (check-equal? out-test '(3 2 1))
  (define out-test2 (for/fold ([out null] #:result out) ([i '(1 2 3)]) (cons i out)))
  (check-equal? out-test2 '(3 2 1))
  (check-equal? (match-triples #:s "s0" #:p "p0")
                (list (triple (URI "s0") (URI "p0") (URI "o0"))))
  (check-equal? (match-triples #:p "p0" #:o "o1")
                (list (triple (URI "o0") (URI "p0") (URI "o1"))))
  (check-equal? (sort (match-triples #:s "s0" #:p "p0" #:transitive #t)
                      string<?
                      #:key (compose gen-value triple-s))
                (list (triple (URI "o0") (URI "p0") (URI "o1"))
                      (triple (URI "s0") (URI "p0") (URI "o0"))))
  (check-equal? (sort (match-triples #:o "o1" #:p "p0" #:transitive #t)
                      string<?
                      #:key (compose gen-value triple-s))
                (list (triple (URI "o0") (URI "p0") (URI "o1"))
                      (triple (URI "s0") (URI "p0") (URI "o0")))))

(define-syntax (position stx)
  (syntax-parse stx
    [(_ func:id store:id)
     #'(remove-duplicates (map func store))]))

(define (subjects store)
  (position triple-s store))

(define (predicates store)
  (position triple-p store))

(define (objects store)
  (position triple-o store))

;;; iri helpers

(define-for-syntax (format-or-string-append fstring)
  (if (string-contains? fstring "~a")
      #'format
      #'string-append))

(module+ test
  (begin-for-syntax
    (check-equal?
     (syntax->datum (format-or-string-append "should be string-append"))
     'string-append)
    (check-equal?
     (syntax->datum (format-or-string-append "should be format ~a"))
     'format)))

(define-for-syntax (add-~a-suffix fstring-stx)
  (let ([fstring (syntax-e fstring-stx)])
    (if (string-contains? fstring "~a")
        fstring-stx
        (datum->syntax fstring-stx (string-append fstring "~a")))))

(define-for-syntax (split-fstring fstring-stx)
  (let ([fstring (syntax-e fstring-stx)])
    (if (string-contains? fstring "~a")
        (syntax->list (datum->syntax fstring-stx (string-split fstring "~a" #:trim? #f)
                                     fstring-stx
                                     fstring-stx))
        (list fstring-stx (syntax/loc fstring-stx "")))))

(module+ test
  (begin-for-syntax
    (check-equal? (map syntax->datum (split-fstring #'"thing ~a middle"))
                  '("thing " " middle"))
    (check-equal? (map syntax->datum (split-fstring #'"thing end~a"))
                  '("thing end" ""))
    (check-equal? (map syntax->datum (split-fstring #'"no thing"))
                  '("no thing" ""))))

(define inverse-lu (make-hash))

(define (qname iri)
  ; TODO if rdf-top is in use skip the (prefix: suffix) and -> prefix:suffix
  ; TODO trie
  (let ([out
         (for/first
             ([(pref func) (in-dict (sort (sequence->list (in-hash-pairs (iri-prefixes)))
                                          ; note that we use > here so that we don't have
                                          ; to call (reverse x)
                                          (λ (a b) (> (string-length a) (string-length b)))
                                          #:key car))]
              #:when (string-prefix? iri pref))
           (let* ([maybe-suffix (string-trim iri pref)]
                  [out maybe-suffix]   ; TODO need the suffixes too
                  )
             (if (non-empty-string? out) (list func out)
                 (list func))))])
    (if out out iri)))

(define-syntax (define-id-funcs stx)
  ; FIXME define-prefixes define-iri-prefixes is the simple case
  ; but we can allow this to be much more complex including (complex: id-part-1 id-part-2 ... id-part-n)
  (syntax-parse stx
    [(_ [prefix:id (~or* fstring:str function:expr)] ...)
     #:with (prefix-colon ...) (map (λ (p) (format-id p #:source p "~a:" (syntax-e p)))
                                    (syntax->list #'(prefix ...)))
     ;#:with (sfunc ...) (map format-or-string-append (syntax->datum #'(fstring ...)))
     #:with (normalized-fstring ...) (map add-~a-suffix (syntax->list #'(fstring ...)))
     #:with ((iri-prefix iri-suffix) ...) (map split-fstring (syntax->list #'(fstring ...)))
     #'(begin (define (prefix-colon [id ""])
                (~? function (format normalized-fstring id))) ...
              ; NOTE we are assuming that all prefixes are unique
              ; FIXME does this overwrite or no?
              (hash-union! (iri-prefixes) (hash (~@ iri-prefix 'prefix-colon) ...))
              )]))

; TODO closed namespaces

(module+ test
  (define-id-funcs
    [a "test"]
    [b "another ~a test"]
    [c "hello"]
    )
  (check-equal? (a: "thing") "testthing")
  (check-equal? (b: 'another-thing) "another another-thing test")
  (check-equal? (c: 199291) "hello199291")

  (define-id-funcs
    [d "hrm"])

  (check-equal? (d: "wat") "hrmwat")

  (check-equal? (qname "hrmhello") '(d: "hello"))

  (check-equal? (qname "test") '(a:))
  (check-equal? (a:) "test")
  (parameterize ([iri-prefixes (make-hash)])
    (define-id-funcs [d "not-hrm/"])
    (check-equal? (qname "test") "test")
    (check-equal? (qname "not-hrm/thing") '(d: "thing")))

  (check-equal? (gen-short (URI "testsuffix")) '(a: "suffix"))

  (define-id-funcs
    [x "x/"]
    [y "x/y/"])
  (check-equal? (gen-short (URI "x/y/longer")) '(y: "longer"))
  (check-equal? (gen-short (URI "x/shorter")) '(x: "shorter"))
  )

;;; rdf #%top

#; ; i think these should be almost exactly equivalent
(define-syntax (rdf-top stx)
  (syntax-parse stx
    [(_ . identifier:id)
     ; reminder that code here is 'free' at runtime becuase it goes at compile time
     #:attr prefix-suffix-stx
     (if (identifier-binding #'identifier)
         #f  ; if the identifier is bound it takes priority
         (let ([id (symbol->string (syntax-e #'identifier))])
           (if (string-contains? id ":")
               (let* ([prefix-suffix (string-split id ":")]
                      [prefix (car prefix-suffix)]
                      [prefix-colon (format-id #'identifier #:source #'identifier
                                               "~a:" (datum->syntax #f (string->symbol prefix)))])
                 (if (identifier-binding (datum->syntax this-syntax prefix-colon))
                     #`(#,prefix-colon #,(cadr prefix-suffix))
                     #f))
               #f)))
     #'(~? prefix-suffix-stx (#%top . identifier))]))

(define-syntax (rdf-top stx)
  (syntax-parse stx
    [(_ . identifier:id)
     (if (identifier-binding #'identifier)
         #'(#%top . identifier)
         (let ([id (symbol->string (syntax-e #'identifier))])
           (if (string-contains? id ":")
               (let* ([prefix-suffix (string-split id ":")]
                      [prefix (car prefix-suffix)]
                      [prefix-colon (format-id #'identifier #:source #'identifier
                                               "~a:" (datum->syntax #f (string->symbol prefix)))])
                 (if (identifier-binding prefix-colon)
                     #`(#,prefix-colon #,(cadr prefix-suffix))
                     ; FIXME figure out how to reorder if statements for a single base case
                     #'(#%top . identifier)
                     ))
               #'(#%top . identifier))))]))

(module+ test
  (define (rdf: value)
    (format "rdf-prefix#~a" value))

  (check-equal? (rdf-top . rdf:type) "rdf-prefix#type")
  (define hello 123)
  (check-equal? (rdf-top . hello) 123))
