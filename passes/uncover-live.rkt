#lang racket

(require "./utils.rkt")
(require graph)

(provide uncover-live)

(define caller-save-regs (apply set (map (lambda (r) `(reg ,r)) (set->list caller-save))))

(define (node? x)
  (match x
    [`(var ,_) #t]
    [`(reg ,_) #t]
    [else #f]))
(define (opr->node-set . oprs)
  (apply set (filter node? oprs)))
(define (W inst)
  (match inst
    [`(pushq ,a) (set)]
    [`(cmpq ,a ,b) (set)]
    [`(,(? indirect-call?) ,f) caller-save-regs]
    [`(callq ,f) caller-save-regs]
    [`(,opt ,oprs ..1) (opr->node-set (last oprs))]
    [else (set)]
    ))
(define (R inst)
  (match inst
    [`(popq ,a) (set)]
    [`(cmpq ,a ,b) (opr->node-set a b)]
    [`(movq ,a ,b) (opr->node-set a)]
    [`(movzbq ,a ,b) (opr->node-set a)]
    [`(leaq ,a ,b) (opr->node-set a)]
    [`(,(? indirect-call?) ,f) (opr->node-set f)]
    [`(,opt ,oprs ...) (apply opr->node-set oprs)]
    [else (set)]
    ))
(define (live-before inst las)
  (set-union (R inst) (set-subtract las (W inst))))
(define (ul-insts insts live-after-set)
  (foldr (lambda (inst sets)
           (cons (live-before inst (car sets)) sets))
         `(,live-after-set) insts))

(define (ul-def def)
  (define cfg '())
  (define live-befores (make-hash))
  (define live-sets (make-hash))
  (define stop #f)
  (define (ul-block bl insts)
    (let* ([succ-bls (get-neighbors cfg bl)]
           [las (foldl (lambda (x acc) (set-union (hash-ref! live-befores x set) acc)) (set) succ-bls)]
           [lss (ul-insts insts las)]
           [lbs (car lss)])
      (unless (equal? lbs (hash-ref! live-befores bl set))
        (set! stop #f))
      (hash-set! live-befores bl lbs)
      (hash-set! live-sets bl lss)))
  (define (flow-iter bls instss)
    (while (not stop)
      (set! stop #t)
      (for ([bl bls]
            [insts instss])
        (ul-block bl insts))))
  (match def
    [`(define ,sig : ,rt ,infos ((,bls . (block ,biss ,instss ...)) ...))
     (set! cfg (dict-ref infos 'cfg))
     (flow-iter bls instss)
     `(define ,sig : ,rt ,infos ,(map (lambda (bl bis insts)
                              `(,bl . (block ((lives . ,(hash-ref! live-sets bl set)) ,@bis) ,@insts)))
                            bls biss instss))]))

(define (uncover-live prog)
  (match prog
    [`(program ,infos ,defs ...)
     `(program ,infos ,@(map ul-def defs))]
    ))
