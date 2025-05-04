#lang racket/base
(require racket/string
         rdf/utils
         racket/list
         (for-syntax racket/base
                     syntax/parse
                     racket/pretty
                     racket/format
                     )
         ;racket/dict
         ; FIXME store klobbering
         ;"test.ttl"
         ;"../../../../git/pyontutils/test/nasty.ttl"

         ;"../../../../../../tmp/test.nt"
         ;"../../../../../../tmp/test.ttl"
         ;(rename-in "../../../../../../tmp/NIF-Molecule.rkt" [store mol-store])
         ;(rename-in "../../../../../../tmp/scicrunch-registry.rkt" [store scr-store])
         )


;; write an nt file because is easier for testing
(define (ser triple out-port)
  (let ([s (triple-s triple)]
        [p (triple-p triple)]
        [o (triple-o triple)])
    ;(cond [(URI? s) (display (bracket (URI-value s)) out-port)]
          ;[(BNode? s) (display (string-append "_:" (BNode-value s))
                               ;out-port)])
    (if (list? s)
        (display "<http://TODO.list>" out-port)
        (display (gen-ser s) out-port))
    (display " " out-port)
    ;(display (bracket (URI-value p)) out-port)
    (display (gen-ser p) out-port)
    (display " " out-port)
    (if (list? o)
        (display "<http://TODO.list>" out-port)
        (display (gen-ser o) out-port))
    ;(cond [(URI? o) (display (bracket (URI-value o)) out-port)]
    ;[(BNode? o) (display (string-append "_:" (BNode-value o))
    ;out-port)]
    ;[(Literal? o) (write "TODO" out-port)]
    ;[(list? o) (write "List TODO" out-port)]
    ;[else (write "WHAT THE FUCK" out-port)]
    ;)
    (display " .\n" out-port)))

(module+ needs-nif-ontology
  #;
  (require NIF-Ontology/rkt/scicrunch-registry)
  (define-id-funcs
    [ilxtr "http://uri.interlex.org/tgbugs/uris/readable/"]
    [owl "http://www.w3.org/2002/07/owl#"]
    [rdf "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
         (type)]
    [rdfs "http://www.w3.org/2000/01/rdf-schema#"])
  (define store
    (list
     (triple (URI (ilxtr: 'thing-0)) (URI (rdf: 'type)) (URI (owl: 'Thing)))
     (triple (URI (ilxtr: 'thing-1)) (URI (rdf: 'type)) (URI (owl: 'Thing)))
     (triple (URI (ilxtr: 'thing-2)) (URI (rdf: 'type)) (URI (owl: 'Thing)))
     (triple (URI (ilxtr: 'thing-3)) (URI (rdf: 'type)) (URI (owl: 'Thing)))
     (triple (URI (ilxtr: 'thing-4)) (URI (rdf: 'type)) (URI (owl: 'Thing)))
     (triple (URI (ilxtr: 'thing-5)) (URI (rdf: 'type)) (URI (owl: 'Thing)))
     (triple (URI (ilxtr: 'thing-6)) (URI (rdf: 'type)) (URI (owl: 'Thing)))
     (triple (URI (ilxtr: 'thing-7)) (URI (rdf: 'type)) (URI (owl: 'Thing)))
     (triple (URI (ilxtr: 'thing-8)) (URI (rdf: 'type)) (URI (owl: 'Thing)))
     (triple (URI (ilxtr: 'thing-9)) (URI (rdf: 'type)) (URI (owl: 'Thing)))
     (triple (SCR: 000200) (URI (rdf: 'type)) (URI (owl: 'NamedIndividual)))
     (triple (SCR: 010200) (URI (rdf: 'type)) (URI (owl: 'NamedIndividual)))
     ))
  (define (make-nt out-port)
    ;(displayln "#lang rdf/nt" out-port)
    (for ([triple store]) (ser triple out-port)))

  (define (things)
    (for/list ([t store] #:when
                         (eq? "<bbbbbbbb>" (if (list? (triple-s t))
                                               #f
                                               (gen-ser (triple-s t)))))
      t)

    (for/list ([t store] #:when (URI? (triple-s t))) t)
    )

  (take (filter (p-eq? rdf:type) store) 10)
  (filter (s-eq? (SCR: 000200)) store)
  (filter (s-eq? (SCR: 010200)) store)
)

(define (read-test)
  (read (current-input-port)))

(define (read-thing)
  ; reading this is super fast
  (define read-in-vals (with-input-from-file
                         "/tmp/would-you-look-at-that.rkt"
                         read-test))
  read-in-vals)

(define read-in-vals '())

; calling eval on it is slow
;(define ns (make-base-namespace))
(define-namespace-anchor anc)
(define (thing)
  (define ns (namespace-anchor->namespace anc))
  (define vals
    (parameterize ([current-namespace ns])
      (eval read-in-vals)))
  vals)

;(define vals (thing))
;(define store vals)

; TODO generate on the fly
(define rdf:type (URI "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))
(define-syntax (SCR: stx)
  (syntax-parse stx
    [(_ numbers)
     ;(pretty-write (syntax-line #'numbers))
     (with-syntax ([uri (string-append
                         "http://scicrunch.org/resolver/SCR_"
                         (~a (number->string (syntax->datum #'numbers))
                             #:width 6
                             #:align 'right
                             #:left-pad-string "0"))])
     #'(URI uri))
     ]))
; next level reader macros take SCR:anything and expand accordingly
(define (s-eq? value) (λ (triple) (equal? (triple-s triple) value)))
(define (p-eq? value) (λ (triple) (equal? (triple-p triple) value)))
(define (o-eq? value) (λ (triple) (equal? (triple-o triple) value)))

(define (-match #:s [s null] #:p [p null] #:o [o null])
  (void))
;(for/list ([t store] #:when (eq? rdf:type (URI-value (triple-p t))))  t)
;(define store mol-store)

;
;(call-with-output-file* "/tmp/test.nt" make-nt #:exists 'replace)
