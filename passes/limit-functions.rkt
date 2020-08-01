#lang racket

(require "./utils.rkt")
(require "./type-tag.rkt")

(provide limit-functions)

(define (lf-exp e)
  (match e
    [`(has-type ,e ,t) `(has-type ,(lf-exp e) ,t)]
    [`(call ,f ,args ...)
     (cond [(<= (length args) 6) e]
           [else (let* ([cut-args (list-tail args 5)]
                        [pack-arg `(vector ,@cut-args)]
                        [pack-arg (type-tag pack-arg)]
                        [real-args `(,@(take args 5) ,pack-arg)])
                   `(call ,f ,@real-args))]
           )]
    [`(,es ...) (map lf-exp es)]
    [else e]
    ))

(define (lf-def def)
  (match def
    [`(define ,(app pbind sig `(,f [,args : ,types] ...)) : ,rt ,infos ,body)
     (set! body (lf-exp body))
     (cond [(<= (length args) 6) `(define ,sig : ,rt ,infos ,body)]
           [else
            (define cut-args (list-tail args 5))
            (define cut-types (list-tail types 5))
            (define pack-arg (gen-sym "pack"))
            (define pack-type `(Vector ,@cut-types))
            (define pack `(has-type ,pack-arg ,pack-type))
            (define real-args `(,@(take args 5) ,pack-arg))
            (define real-types `(,@(take types 5) ,pack-type))
            (define real-body (for/foldr ([body body]) ([a cut-args] [t cut-types] [i (in-naturals)])
                                         `(let ([,a (has-type (vector-ref ,pack ,i) ,t)])
                                            ,body)))
            `(define (,f ,@(for/list ([a real-args] [t real-types])
                             `[,a : ,t]))
               : ,rt ,infos
               ,(type-tag real-body))]
           )]
    ))

(define (limit-functions prog)
  (match prog
    [`(program ,infos ,defs ...)
     `(program ,infos ,@(map lf-def defs))]
    ))
