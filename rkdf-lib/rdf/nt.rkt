#lang racket/base
(require syntax/strip-context
         rdf/nt/parser
         rdf/nt/tokenizer)
(require racket/pretty)

(define printed #t)

(define (read-syntax source-name in-port)
  ; why is this called more than once???
  (define parse-tree (parse source-name (nt-make-tokenizer in-port)))
  ;(define parse-tree (parse-to-datum (protc-make-tokenizer port)))
  ;'(when (not printed)
    ;(begin (pretty-write (syntax->datum parse-tree))
           ;(set! printed #t)))
  (define output-syntax (strip-context  ; required to avoid issues with #%app for reasons I don't understand at the moment
  #`(module nt-module rdf/nt/expander  ; TODO this is one place that we swap could out backends
      ; for pdf/html/execution/data input OR a better strat is to
      ; have this expand to an intermediate representation
      ; from which we can then have a suite of racket functions/modules/scripts that transform to the desired output...
      ; yes, the expander here should just deal with correctness of the protocols in question
      ; rexport comes later...
      #,parse-tree)))

  ;(when (not printed)
    ;(begin (pretty-write (syntax->datum output-syntax)) 
           ;(set! printed #t)))
  output-syntax)

(module+ reader
  (provide read-syntax get-info)
  (define (get-info port sourc-module source-line source-collection source-position)
    (define (handle-query key default)
      (case key [else default]))
    handle-query)
  )
