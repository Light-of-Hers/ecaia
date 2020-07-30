#lang racket

(require "utils.rkt")
(require "../utilities.rkt")

(provide patch-instructions)

(define (deref? x)
  (match x
    [`(deref ,_ ,_) #t]
    [else #f]))

(define (pi-inst inst)
  (match inst
    [`(,opt ,(? deref? a) ,(? deref? b))
     `((movq ,a (reg rax)) (,opt (reg rax) ,b))]
    [`(,(? indirect-call? call) ,(? deref? f))
     `((movq ,f (reg rax)) (,call (reg rax)))]
    [`(movq ,a ,b) #:when (equal? a b) '()]
    [else `(,inst)]))

(define (pi-def def)
  (match def
    [`(define ,sig : ,rt ,infos ((,ls . (block ,bis ,instss ...)) ...))
     `(define ,sig : ,rt ,infos
        ,(for/list ([l ls] [bi bis] [insts instss])
           `(,l . (block ,bi ,@(apply append (map pi-inst insts))))))]
    ))

(define (patch-instructions prog)
  (match prog
    [`(program ,infos ,defs ...)
     `(program ,infos ,@(map pi-def defs))]
    ))
