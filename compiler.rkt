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
(require "./utilities.rkt")


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
                    (define (sub1 [x : Integer])
                      : Integer
                      (- x 1))
                    (define (fib-iter [n : Integer] [a : Integer] [b : Integer])
                      : Integer
                      (if (eq? n 0)
                          a
                          (fib-iter (- n 1) b (+ a b))))
                    (define (fib [n : Integer])
                      : Integer
                      (fib-iter n 0 1))
                    (vector-ref (map-vec2 (if (< (read) 0) sub1 add1) (vector 0 (fib (read)))) 1)
                    )
                   ))

;; (pretty-print (remove-complex (shrink prog)))
(define out (apply-passes (list-ref examples 0)
                          type-check
                          expose-allocation
                          uniquify
                          shrink
                          reveal-functions
                          remove-complex
                          explicate-control
                          select-instructions
                          uncover-live
                          build-interference
                          allocate-registers
                          patch-instructions
                          print-x86
                          ))

;; (pretty-display out)
;; (pretty-display (strip-has-type out))
(displayln out)
