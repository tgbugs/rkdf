#lang racket/base
(require debug/repl racket/trace)
(require (for-syntax racket/base
                     syntax/parse)
         rdf/utils
         file/md5
         file/sha1
         racket/generic
         racket/pretty
         racket/list
         racket/dict
         racket/format
         racket/string)

(provide
 (rename-out [nt-module-begin #%module-begin])
 (except-out (all-from-out racket/base) #%module-begin)
 (all-from-out rdf/utils)
 (all-defined-out))

(define-syntax (nt-module-begin stx)
  (syntax-parse stx
    [(_ ast)  ; TODO maybe inject context here?
     #'(#%module-begin
        ; TODO provide with prefix?
        ; or try to find a global store?
        ;(require rdf/utils (for-syntax rdf/utils))
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

(define (ntDoc . rest)
  (set! store (append rest store)))

(define (uriref uri)
  (URI uri))

(define (nodeID node)
  (BNode node))

(define (quotedString . rest)
  (string-join rest ""))

(define (language . rest)
  (string-join rest ""))

(define (bareString string)
  (Literal string null null))

(define (datatypeString string uri)
  (Literal string uri null))

(define (langString string lang)
  (Literal string null lang))

(module+ test
  (triple (BNode "a") (URI "b") (Literal "c" null null))
  )
  
