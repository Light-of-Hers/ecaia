#lang racket

(require "./passes/type-check.rkt")
(require "./passes/uniquify.rkt")
(require "./passes/remove-complex.rkt")
(require "./passes/shrink.rkt")
(require "./passes/explicate-control.rkt")
(require "./passes/select-instructions.rkt")
(require "./passes/uncover-live.rkt")
(require "./passes/build-interference.rkt")
(require "./passes/allocate-registers.rkt")
(require "./passes/patch-instructions.rkt")
(require "./passes/print-x86.rkt")
(require "./passes/expose-allocation.rkt")
(require "./passes/reveal-functions.rkt")
(require "./passes/limit-functions.rkt")
(require "./passes/convert-to-closures.rkt")
(require (only-in "./passes/utils.rkt" strip-has-type))


(define (apply-passes prog . passes)
  (foldl (lambda (pass prog) (pass prog)) prog passes))

(define examples '((program
                    ()
                    (define (map-vec2 [f : (Integer -> Integer)]
                                      [v : (Vector Integer Integer)])
                      : (Vector Integer Integer)
                      (vector (f (vector-ref v 0)) (f (vector-ref v 1))))
                    (define (add1 [x : Integer])
                      : Integer
                      (+ x 1))
                    (let ([diff (read)]) (vector-ref (map-vec2 (if (< (read) 0)
                                                                   (lambda: ([x : Integer]) : Integer (- x diff))
                                                                   add1) (vector 1 (read))) 1))
                    )
                   ))

;; (pretty-print (remove-complex (shrink prog)))
(define out (apply-passes (list-ref examples 0)
                          type-check
                          uniquify
                          shrink
                          reveal-functions
                          convert-to-closures
                          limit-functions
                          expose-allocation
                          remove-complex
                          explicate-control
                          select-instructions
                          uncover-live
                          build-interference
                          allocate-registers
                          patch-instructions
                          print-x86
                          ))

;; (pretty-display out) (newline)
;; (pretty-display (strip-has-type out)) (newline)
(displayln out)
