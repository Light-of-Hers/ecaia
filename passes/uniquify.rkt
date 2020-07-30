#lang racket

(require "../utilities.rkt")
(require "./utils.rkt")

(provide uniquify)

(define (uniquify-exp alist e)
  (define recur ((curry uniquify-exp) alist))
  (match e
    [`(has-type ,e ,t) `(has-type ,(recur e) ,t)]
    [(? symbol?) (dict-ref alist e)]
    [(? fixnum?) e]
    [(? boolean?) e]
    [`(global-value ,_ ...) e]
    [`(allocate ,_ ...) e]
    [`(collect ,_ ...) e]
    [`(let ([_ ,e]) ,body)
     `(let ([_ ,(recur e)]) ,(recur body))]
    [`(let ([,x ,e]) ,body)
     (let* ([new-e (recur e)]
            [new-x (gen-sym (symbol->string x))]
            [new-b (uniquify-exp (cons `(,x . ,new-x) alist) body)])
       `(let ([,new-x ,new-e]) ,new-b))]
    [`(,op ,es ...)
     `(,(if (builtin-op? op) op (recur op)) ,@(map recur es))]
    [else e]
    ))

(define (uniquify-def alist def)
  (match def
    [`(define (,f [,args : ,ats] ...) : ,rt ,body)
     (let* ([new-f (dict-ref alist f)]
            [new-args (map (lambda (arg) (gen-sym (symbol->string arg))) args)]
            [new-alist (foldr (lambda (o n l) (cons `(,o . ,n) l)) alist args new-args)]
            [new-body (uniquify-exp new-alist body)])
       `(define (,new-f ,@(map (lambda (a t) `[,a : ,t]) new-args ats)) : ,rt ,new-body))]
    ))

(define (collect-def-names defs)
  (foldr (lambda (def alist)
           (match def
             [`(define (,f ,_ ...) : ,_ ,_)
              `((,f . ,(gen-sym (symbol->string f))) ,@alist)]
             ))
         '() defs))

(define (uniquify prog)
  (match prog
    [`(program ,info ,defs ... ,body)
     (define alist (collect-def-names defs))
     `(program ,info ,@(map ((curry uniquify-def) alist) defs) ,(uniquify-exp alist body))]
    ))

