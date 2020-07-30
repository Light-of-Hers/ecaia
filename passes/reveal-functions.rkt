#lang racket

(require "./utils.rkt")
(require "../utilities.rkt")

(provide reveal-functions)

(define (rf-exp f-names e)
  (define recur ((curry rf-exp) f-names))
  (match e
    [`(has-type ,e ,t) `(has-type ,(recur e) ,t)]
    [(? symbol?) (if (set-member? f-names e) `(fun-ref ,e) e)]
    [(? simple?) e]
    [`(let ([,x ,e]) ,body) `(let ([,x ,(recur e)]) ,(recur body))]
    [`(,op ,es ...) #:when (builtin-op? op) `(,op ,@(map recur es))]
    [`(,_ ,_ ...) `(call ,@(map recur e))]
    [else e]
    ))

(define (rf-def f-names def)
  (match def
    [`(define ,sig : ,rt ,infos ,body)
     `(define ,sig : ,rt ,infos ,(rf-exp f-names body))]
    ))

(define (collect-fun-names defs)
  (foldr (lambda (d s)
           (match d
             [`(define (,f ,_ ...) ,_ ...) (set-add s f)]))
         (set) defs))

(define (reveal-functions prog)
  (match prog
    [`(program ,infos ,defs ...)
     (define f-names (collect-fun-names defs))
     `(program ,infos ,@(map ((curry rf-def) f-names) defs))]
    ))
