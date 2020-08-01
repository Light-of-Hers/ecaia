#lang racket

(require "./utils.rkt")
(require "./type-tag.rkt")

(provide expose-allocation)

(define (ec-simple? e)
  (match e
    [`(has-type ,(? arg?) ,_) #t]
    [else #f]
    ))

(define (ec-alloc es type)
  (define len (length es))
  (define is (range len))
  (define xs (map (lambda (e i)
                    (if (ec-simple? e) e (gen-sym "x")))
                  es is))
  (define v (gen-sym "v"))
  (define bytes (* 8 (+ len 1)))
  (define e-init (foldr (lambda (i x body)
                          `(let ([_ (vector-set! ,v ,i ,x)]) ,body))
                        v is xs))
  (define e-alloc `(let ([_ (if (< (+ (global-value free_ptr) ,bytes)
                                   (global-value fromspace_end))
                                (void)
                                (collect ,bytes))])
                     (let ([,v (allocate ,len ,type)]) ,e-init)))
  (define e-binds (foldr (lambda (x e body)
                           (if (ec-simple? e)
                               body
                               `(let ([,x ,(ec-exp e)]) ,body)))
                         e-alloc xs es))
  (type-tag e-binds))

(define (ec-exp e [t '()])
  (match e
    [`(has-type ,e ,t) `(has-type ,(ec-exp e t) ,t)]
    [(? arg?) e]
    [`(vector ,es ...)
     (ec-alloc es t)]
    [`(let ([,x ,e]) ,body)
     `(let ([,x ,(ec-exp e)]) ,(ec-exp body))]
    [`(,op ,es ...) `(,(ec-exp op) ,@(map ec-exp es))]
    ))

(define (ec-def def)
  (match def
    [`(define ,sig : ,rt ,infos ,body)
     `(define ,sig : ,rt ,infos ,(ec-exp body))]
    ))

(define (expose-allocation prog)
  (match prog
    [`(program ,infos ,defs ...)
     `(program ,infos ,@(map ec-def defs))]
    ))
