#lang racket

(require "../utilities.rkt")
(require "./utils.rkt")

(provide shrink)

(define (sk-exp e)
  (match e
    [`(has-type ,(app sk-exp e1) ,t) `(has-type ,e1 ,t)]
    [`(let ([,x ,(app sk-exp e1)]) ,(app sk-exp body))
     `(let ([,x ,e1]) ,body)]
    [`(- ,(app sk-exp a) ,(app sk-exp b)) `(+ ,a (has-type (- ,b) Integer))]
    [`(and ,(app sk-exp a) ,(app sk-exp b)) `(if ,a ,b (has-type #f Boolean))]
    [`(or ,(app sk-exp a) ,(app sk-exp b)) `(if ,a (has-type #t Boolean) ,b)]
    [`(,op ,es ...) `(,(sk-exp op) ,@(map sk-exp es))]
    [else e]))

(define (sk-def def)
  (match def
    [`(define ,sig : ,rt ,body)
     `(define ,sig : ,rt () ,(sk-exp body))]
    ))

(define (shrink prog)
  (match prog
    [`(program ,infos ,defs ... ,body)
     `(program ,infos
               ,@(map sk-def defs)
               (define (main) : Integer () ,(sk-exp body)))]))
