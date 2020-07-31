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
                    (define (sum10 [a1 : Integer] [a2 : Integer] [a3 : Integer] [a4 : Integer] [a5 : Integer]
                                   [a6 : Integer] [a7 : Integer] [a8 : Integer] [a9 : Integer] [a10 : Integer])
                      : Integer
                      (+ a1 (+ a2 (+ a3 (+ a4 (+ a5 (+ a6 (+ a7 (+ a8 (+ a9 a10))))))))))
                    (vector-ref (map-vec2 (if (< (read) 0) sub1 add1) (vector 1 (fib (read)))) 1)
                    )
                   ))

;; (pretty-print (remove-complex (shrink prog)))
(define out (apply-passes (list-ref examples 0)
                          type-check
                          uniquify
                          shrink
                          reveal-functions
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

;; (pretty-display out)
;; (pretty-display (strip-has-type out))
(displayln out)
