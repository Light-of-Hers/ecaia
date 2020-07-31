#lang racket

(require "./utils.rkt")

(provide remove-complex)

(define (rc-arg e [t '()])
  (define (lift e) `(has-type ,e ,t))
  (match e
    [`(has-type ,e ,t) (rc-arg e t)]
    [(? simple?) (values (lift e) '())]
    [`(let ([,x ,e]) ,body)
     (let-values ([(res binds) (rc-arg body)])
       (values res (cons `(,x . ,(rc-exp e)) binds)))]
    [`(if ,cnd ,thn ,els)
     (let ([tmp (gen-sym "tmp")])
       (values (lift tmp) `((,tmp . (has-type (if ,(rc-exp cnd) ,(rc-exp thn) ,(rc-exp els)) ,t)))))]
    [`(,op ,es ...)
     (let-values ([(ress bindss) (for/lists (_1 _2) ([e es]) (rc-arg e))])
       (let ([tmp (gen-sym "tmp")])
         (values (lift tmp) `(,@(apply append bindss) (,tmp . (has-type (,op ,@ress) ,t))))))]
    ))

(define (serialize-bindings binds tail t)
  (foldr (lambda (bind body)
           `(let ([,(car bind) ,(cdr bind)]) (has-type ,body ,t)))
         tail binds))

(define (rc-exp e [t '()])
  (match e
    [`(has-type ,e ,t) `(has-type ,(rc-exp e t) ,t)]
    [(? simple?) e]
    [`(let ([,x ,e]) ,body)
     `(let ([,x ,(rc-exp e)]) ,(rc-exp body))]
    [`(if ,cnd ,thn ,els)
     `(if ,(rc-exp cnd) ,(rc-exp thn) ,(rc-exp els))]
    [`(,op ,es ...)
     #:when (builtin-op? op)
     (let-values ([(ress bindss) (for/lists (_1 _2) ([e es]) (rc-arg e))])
       (serialize-bindings (apply append bindss) `(,op ,@ress) t))]
    [`(,_ ,_ ...)
     (let-values ([(ress bindss) (for/lists (_1 _2) ([e e]) (rc-arg e))])
       (serialize-bindings (apply append bindss) ress t))]
    ))

(define (rc-def def)
  (match def
    [`(define ,sig : ,rt ,infos ,body)
     `(define ,sig : ,rt ,infos ,(rc-exp body))]
    ))

(define (remove-complex prog)  
  (match prog
    [`(program ,infos ,defs ...)
     `(program ,infos ,@(map rc-def defs))]
    ))
