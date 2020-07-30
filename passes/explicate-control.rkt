#lang racket

(require "../utilities.rkt")
(require "./utils.rkt")
(provide explicate-control)

(define (explicate-control prog)
  (define locals (make-hash))
  (define (ec-def def)
    (define blocks '())
    (define (new-block e [l "label"])
      (let ([label (gen-sym l)])
        (set! blocks `((,label . ,e) ,@blocks))
        label))
    (define (ec-pred cnd l-then l-else)
      (match cnd
        [`(has-type ,e ,t) (ec-pred e l-then l-else)]
        [`(if ,cnd1 ,thn1 ,els1)
         (let* ([l-then1 (new-block (ec-pred thn1 l-then l-else) "then")]
                [l-else1 (new-block (ec-pred els1 l-then l-else) "else")])
           (ec-pred cnd1 l-then1 l-else1))]
        [`(let ([,x ,e]) ,body)
         (ec-assign x e (ec-pred body l-then l-else))]
        [else `(if ,(strip-has-type cnd) (goto ,l-then) (goto ,l-else))]
        ))
    (define (ec-assign x e body [t '()])
      (match e
        [`(has-type ,e ,t) (ec-assign x e body t)]
        [`(if ,cnd ,thn ,els)
         (let* ([l-merge (new-block body "merge")]
                [l-then (new-block (ec-assign x thn `(goto ,l-merge)) "then")]
                [l-else (new-block (ec-assign x els `(goto ,l-merge)) "else")])
           (ec-pred cnd l-then l-else))]
        [`(let ([,x1 ,e1]) ,body1)
         (ec-assign x1 e1 (ec-assign x body1 body))]
        [else
         (hash-set! locals x t)
         `(seq (assign ,x ,(strip-has-type e)) ,body)]
        ))
    (define (ec-tail e)
      (match e
        [`(has-type ,e ,_) (ec-tail e)]
        [`(if ,cnd ,thn ,els)
         (let* ([l-then (new-block (ec-tail thn) "then")]
                [l-else (new-block (ec-tail els) "else")])
           (ec-pred cnd l-then l-else))]
        [`(let ([,x ,e]) ,body)
         (ec-assign x e (ec-tail body))]
        [`(call ,es ...) `(tailcall ,@(strip-has-type es))]
        [else `(return ,(strip-has-type e))]
        ))
    (match def
      [`(define ,(app pbind sig `(,f [,args : ,ts] ...)) : ,rt ,infos ,body)
       (for ([arg args] [t ts])
         (hash-set! locals arg t))
       (set! body (ec-tail body))
       (define start (gen-sym (format "~a-start" f)))
       (define conclusion (gen-sym (format "~a-conclusion" f)))
       (set! blocks `((,start . ,body) ,@blocks))
       `(define ,sig : ,rt ((start . ,start) (conclusion . ,conclusion) ,@infos) ,blocks)]
      ))
  (match prog
    [`(program ,infos ,defs ...)
     (set! defs (map ec-def defs))
     `(program ((locals . ,locals) ,@infos) ,@defs)]
    ))

