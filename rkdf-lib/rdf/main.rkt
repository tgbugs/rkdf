#lang racket/base
(require syntax/strip-context
         rdf/turtle/parser
         (rename-in rdf/turtle/tokenizer
                    ; TODO ?
                    [turtle-make-tokenizer rdf-make-tokenizer]))
(require racket/pretty)


(define printed #t)

(define (read-syntax source-name in-port)
  ; why is this called more than once???
  (define parse-tree (parse source-name (rdf-make-tokenizer in-port)))
  ;(define parse-tree (parse-to-datum (protc-make-tokenizer port)))
  '(when (not printed)
    (begin (pretty-write (syntax->datum parse-tree))
           (set! printed #t)))
  (define output-syntax (strip-context  ; required to avoid issues with #%app for reasons I don't understand at the moment
  #`(module rdf-module rdf/expander  ; TODO this is one place that we swap could out backends
      ; for pdf/html/execution/data input OR a better strat is to
      ; have this expand to an intermediate representation
      ; from which we can then have a suite of racket functions/modules/scripts that transform to the desired output...
      ; yes, the expander here should just deal with correctness of the protocols in question
      ; rexport comes later...
      #,parse-tree))
    )
  (when (not printed)
    (begin (pretty-write (syntax->datum output-syntax)) 
           (set! printed #t)))
  output-syntax)

(module+ reader
  (provide read-syntax get-info)
  (define (get-info port sourc-module source-line source-collection source-position)
    (define (handle-query key default)
      (case key [else default]))
    handle-query)
  )
