#lang racket

(provide arg? arg->opr pbind builtin-op? simple? gen-sym)

(define (arg? x)
  (match x
    [(? symbol?) #t]
    [(? fixnum?) #t]
    [(? boolean?) #t]
    [`(global-value ,_) #t]
    [else #f]
    ))

(define (simple? e)
  (match e
    [`(global-value ,_) #t]
    [`(allocate ,_ ...) #t]
    [`(collect ,_ ...) #t]
    [(? arg?) #t]
    [else #f]
    ))

(define (arg->opr x)
  (match x
    [(? symbol?) `(var ,x)]
    [(? fixnum?) `(int ,x)]
    [(? boolean?) `(int ,(if x 1 0))]
    [`(global-value ,_) x]
    ))

(define (pbind x) (values x x))

(define builtin-ops (set '+ '- '< '> 'eq? '<= '>= 'and 'or 'not 'read 'if
                         'vector 'vector-ref 'vector-set! 'void 'collect 'allocate 'global-value
                         'fun-ref 'call 'tailcall))
(define (builtin-op? op)
  (set-member? builtin-ops op))


(define (gen-sym s)
  (gensym (string-replace s "-" "_")))
