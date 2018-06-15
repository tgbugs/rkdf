#lang racket/base
(require racket/generic
         racket/string)
(provide (all-defined-out)
         (struct-out Literal)
         (struct-out triple)
         (struct-out BNode)
         (struct-out URI))

(define (bracket uri)
  (string-append "<" uri ">"))

(define-generics serializable
  ; TODO consider using current output port to do this directly
  (gen-ser serializable)
  (gen-value serializable)
  )

(struct URI (value) #:inspector #f
  #:methods gen:serializable
  [(define (gen-value uri) (URI-value uri))
   (define (gen-ser uri)
     (string-append (bracket (URI-value uri))))])

(struct triple (s p o) #:inspector #f
  #:methods gen:serializable
  ; TODO
  [(define (gen-ser trip)
     trip)
   ])

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

