#lang racket/base
(require debug/repl racket/trace)
(require (for-syntax racket/base
                     syntax/parse)
         rdf/utils
         file/md5
         file/sha1
         racket/pretty
         racket/list
         racket/dict
         racket/format
         racket/string)
(provide
 (rename-out [turtle-module-begin #%module-begin])
 (except-out (all-from-out racket/base) #%module-begin)
 (all-from-out rdf/utils)
 (all-defined-out))

(define-syntax (turtle-module-begin stx)
  (syntax-parse stx
    [(_ ast)  ; TODO maybe inject context here?
     #'(#%module-begin
        ; TODO provide with prefix?
        ; or try to find a global store?
        (require rdf/utils)
        (provide base prefixes store
                 ;gen-ser
                 (all-from-out rdf/utils)
                 ;(struct-out Literal)
                 ;(struct-out triple)
                 ;(struct-out BNode)
                 ;(struct-out URI)
                 )
        ast)]))

(define base null)

(define prefixes
  '(("xsd"."http://www.w3.org/2001/XMLSchema#")))
(define store '())
(define start (md5 (number->string (current-inexact-milliseconds)) #f))

(define (bnode)
  ; TODO see if this matches spec?
  (let ([out (BNode (bytes->hex-string start))])
    (set! start (md5 start #f))
    out))

(define (relativeURI . rest)
  (string-join rest ""))

;(define (nameChar char)
;char)

(define (turtleDoc . rest)
  ; TODO cull voids
  ; TODO macro that defines
  ; the relevant bits
  ;(pretty-write prefixes)
  (set! store (append
               (apply append (filter (λ (thing) (not (void? thing))) rest))
               store)))

(define (baseID iri)
  (if (string-prefix? iri "http://")
      (set! base iri)
      (raise-syntax-error 'bad-base (~a "bad base" iri))
      ))

(define (prefixID prefix iri)
  (set! prefixes (cons (cons prefix iri) prefixes)))

(define (prefixName . rest)
  (string-join rest ""))

(define (name . rest)
  (string-join rest ""))

(define (qnamef prefix . suffix)
  (string-append
   (dict-ref prefixes prefix)
   (if (empty? suffix)
       ""
       (car suffix))))

(define (qnameb suffix)
  (cond [(dict-ref prefixes "" #f)
         (string-append (dict-ref prefixes "") suffix)]
        [(not (null? base)) (string-append base suffix)]
        [else (raise-syntax-error
               'no-base-nor-empty-prefix "no base nor empty prefix")]
        ))

(define (resource uri)
  (cond
    [(string-prefix? uri "http://") (URI uri)]
    [(not (null? base)) (URI (string-append base uri))]
    [else (raise-syntax-error
            'no-base "no base")]
    ))

;(define (objectList . objects)
;(cons 'olist objects)
;objects)

(define (triples subject . polists)
  ;(debug-repl)
  ;(pretty-print `(triples: ,polists))
  (for/list ([po (apply append polists)])
    (triple subject (car po) (cdr po))))

;(define (predicateObjectList . polists)
;(apply append polists))

(define (verb p-or-a)
  (if (eq? p-or-a "a")
      (resource (qnamef "rdf" "type"))
      p-or-a))

(define (polist pred . objs)
  ;(pretty-write `(polist: ,pred ,objs))
  (let ([out (for/list ([o objs]) (cons pred o))])
    ;(pretty-print out)
    out))

;(define (olist . rest)
;rest)

(define (quotedString . rest)
  (string-join rest ""))

(define (language . rest)
  (string-join rest ""))

(define (collection . rest)
  rest)

(define (boolean value)
  ; TODO convert to native datatypes for export/use
  (if (eq? value "true")
      (Literal #t (resource (qnamef "xsd" "boolean")) null)
      (Literal #f (resource (qnamef "xsd" "boolean")) null)))

(define (bareString string)
  (Literal string null null))

(define (datatypeString string uri)
  (Literal string uri null))

(define (langString string lang)
  (Literal string null lang))

(define (integer . str)
  (Literal (string->number (string-join str ""))
           (resource (qnamef "xsd" "integer"))
           null)) 

(define (decimal . str)
  (Literal (string->number (string-join str ""))
           (resource (qnamef "xsd" "decimal"))
           null))

(define (double . str)
  (Literal (string->number (string-join str ""))
           (resource (qnamef "xsd" "double"))
           null))

(define (square . polists)
  (let ([node (bnode)])
    (set! store (append (apply triples node polists) store))
    node))

(module+ test
  (triple (BNode "a") (URI "b") (Literal "c" null null))
  )
