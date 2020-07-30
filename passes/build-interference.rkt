#lang racket

(require "utils.rkt")
(require "../utilities.rkt")
(require graph)

(provide build-interference)

(define (build-interference prog)
  (define g (unweighted-graph/undirected '()))
  (define locals '())
  (define (Vector? v)
    (match v
      [`(var ,v) (match (hash-ref locals v)
                   [`(Vector ,_ ...) #t]
                   [else #f])]
      [else #f]
      ))
  (define (node? x)
    (match x
      [`(var ,_) #t]
      [`(reg ,r) #:when (vector-member r general-registers) #t]
      [else #f]))
  (define (conflict! a b)
    (when (and (node? a) (node? b))
      (add-edge! g a b)))
  (define (bi-inst inst live-after-set)
    (match inst
      [`(,mov ,s ,d) #:when (or (eq? mov 'movq) (eq? mov 'movzbq) (eq? mov 'leaq))
       (for ([v live-after-set]
             #:unless (or (equal? d v) (equal? s v)))
         (conflict! d v))]
      [`(cmpq ,a ,b) (void)]
      [`(callq ,f)
       (for* ([r caller-save]
              [v live-after-set])
         (conflict! `(reg ,r) v))
       (when (eq? f 'collect)
         (for* ([v live-after-set]
                #:when (Vector? v)
                [r callee-save])
           (conflict! `(reg ,r) v)))]
      [`(,(? indirect-call?) ,f)
       (for* ([r caller-save] [v live-after-set])
         (conflict! `(reg ,r) v))
       (for* ([v live-after-set] #:when (Vector? v)
              [r callee-save])
         (conflict! `(reg ,r) v))]
      [`(,opt ,oprs ..1)
       (let ([d (last oprs)])
         (for ([v live-after-set]
               #:unless (equal? d v))
           (conflict! d v)))]
      [else (void)]
      ))
  (define (bi-def def)
    (match def
      [`(define ,sig : ,rt ,infos ,(app pbind blks `((,_ . (block ,b-infos ,instss ...)) ...)))
       (for ([bi b-infos] [insts instss])
         (define live-after-set (cdr (dict-ref bi 'lives)))
         (for-each bi-inst insts live-after-set))]
      ))
  (match prog
    [`(program ,infos ,defs ...)
     (let ([lcs (dict-ref infos 'locals)])
       (hash-remove! lcs '_)
       (set! locals lcs)
       (for ([(v _) lcs]) (add-vertex! g `(var ,v))))
     (for-each bi-def defs)
     ;; (pretty-print (get-edges g))
     `(program ((conflicts . ,g) ,@infos) ,@defs)]
    ))
