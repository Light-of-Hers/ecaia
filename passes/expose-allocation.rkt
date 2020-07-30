#lang racket

(require "./utils.rkt")
(require "../utilities.rkt")
(require "./type-check.rkt")

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
                    (if (ec-simple? e) e (string->symbol (format "x~a" i))))
                  es is))
  (define bytes (* 8 (+ len 1)))
  (define e-init (foldr (lambda (i x body)
                          `(let ([_ (vector-set! v ,i ,x)]) ,body))
                        `v is xs))
  (define e-alloc `(let ([_ (if (< (+ (global-value free_ptr) ,bytes)
                                   (global-value fromspace_end))
                                (void)
                                (collect ,bytes))])
                     (let ([v (allocate ,len ,type)]) ,e-init)))
  (define e-binds (foldr (lambda (x e body)
                           (if (ec-simple? e)
                               body
                               `(let ([,x ,(ec-exp e)]) ,body)))
                         e-alloc xs es))
  (define-values (e-checked _) (type-check-exp '() e-binds))
  e-checked)

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
    [`(define ,sig : ,rt ,body)
     `(define ,sig : ,rt ,(ec-exp body))]
    ))

(define (expose-allocation prog)
  (match prog
    [`(program ,infos ,defs ... ,body)
     `(program ,infos ,@(map ec-def defs) ,(ec-exp body))]
    ))
