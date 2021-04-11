#lang racket

(require "./utils.rkt")
(require graph)

(provide select-instructions)


(define cmp-ops `((eq? . e)
                  (< . l)
                  (> . g)
                  (<= . le)
                  (>= . ge)
                  ))
(define (cmp? op)
  (assoc op cmp-ops))

(define arg-regs '(rdi rsi rdx rcx r8 r9))

(define (gen-tag len ts)
  (define res 0)
  (set! res (bitwise-ior res 1 (arithmetic-shift len 1)))
  (for ([t ts] [i (range len)])
    (match t
      [`(Vector ,_ ...) (set! res (bitwise-ior res (arithmetic-shift 1 (+ i 7))))]
      [else (void)]
      ))
  res)

(define (sel-assign x e)
  (match e
    [`(vector-ref ,(app arg->opr vec) ,n)
     `((movq ,vec (reg r11))
       (movq (deref r11 ,(* 8 (+ n 1))) ,x))]
    [`(vector-set! ,(app arg->opr vec) ,n ,(app arg->opr rhs))
     `((movq ,vec (reg r11))
       (movq ,rhs (deref r11 ,(* 8 (+ n 1)))))]
    [`(allocate ,len (Vector ,ts ...))
     `((movq (global-value free_ptr) ,x)
       (addq (int ,(* 8 (+ len 1))) (global-value free_ptr))
       (movq ,x (reg r11))
       (movq (int ,(gen-tag len ts)) (deref r11 0)))]
    [`(collect ,bytes)
     `((movq (reg r15) (reg rdi))
       (movq (int ,bytes) (reg rsi))
       (callq collect))]
    [`(call ,f ,args ...)
     `(,@(for/list ([a args] [r arg-regs])
           `(movq ,(arg->opr a) (reg ,r)))
       (indirect-callq ,(arg->opr f))
       (movq (reg rax) ,x))]
    [`(fun-ref ,_)
     `((leaq ,e ,x))]
    [`(void) `()]
    [(? arg?) `((movq ,(arg->opr e) ,x))]
    [`(read) `((callq read_int) (movq (reg rax) ,x))]
    [`(- ,(app arg->opr a)) `((movq ,a ,x) (negq ,x))]
    [`(+ ,(app arg->opr a) ,(app arg->opr b))
     (cond [(equal? a x) `((addq ,b ,x))]
           [(equal? b x) `((addq ,a ,x))]
           [else `((movq ,a ,x) (addq ,b ,x))]
           )]
    [`(not ,(app arg->opr a))
     (cond [(equal? a x) `((xorq (int 1) ,x))]
           [else `((movq ,a ,x) (xorq (int 1) ,x))]
           )]
    [`(,(? cmp? op) ,(app arg->opr a) ,(app arg->opr b))
     `((cmpq ,b ,a)
       (set ,(cdr (assoc op cmp-ops)) (byte-reg al))
       (movzbq (byte-reg al) ,x))]
    ))

(define (gen-branch op a b l1 l2)
  (match `(,a ,b)
    [`((int ,a) (int ,b))
     (if ((eval op (make-base-namespace)) a b)
         `((jmp ,l1))
         `((jmp ,l2)))]
    [`((int ,_) ,_)
     `((cmpq ,a ,b)
       (jmp-if ,(cdr (assoc op cmp-ops)) ,l2)
       (jmp ,l1))]
    [else
     `((cmpq ,b ,a)
       (jmp-if ,(cdr (assoc op cmp-ops)) ,l1)
       (jmp ,l2))]
    ))

(define (sel-def def)
  (define cfg (unweighted-graph/directed '()))
  (define start '())
  (define conclusion '())
  (define main? #f)
  (define (sel-tail l t)
    (add-vertex! cfg l)
    (match t
      [`(return ,e) `(,@(sel-assign `(reg rax) e) (jmp ,conclusion))]
      [`(seq (assign ,x ,e) ,t) (append (sel-assign `(var ,x) e) (sel-tail l t))]
      [`(goto ,l1)
       (add-directed-edge! cfg l l1)
       `((jmp ,l1))]
      [`(tailcall ,f ,args ...)
       `(,@(for/list ([a args] [r arg-regs])
             `(movq ,(arg->opr a) (reg ,r)))
         ,@(if main?
               `((indirect-callq ,(arg->opr f))
                 (jmp ,conclusion))
               `((tail-jmp ,(arg->opr f)))))]
      [`(if ,cmp (goto ,l1) (goto ,l2))
       (add-directed-edge! cfg l l1) (add-directed-edge! cfg l l2)
       (match cmp
         [`(,(? cmp? op) ,(app arg->opr a) ,(app arg->opr b))
          (gen-branch op a b l1 l2)]
         [(? arg? x) (gen-branch 'eq? (arg->opr x) `(int 0) l2 l1)]
         [`(not ,x) (gen-branch  'eq? (arg->opr x) `(int 0) l1 l2)]
         )]
      ))
  (match def
    [`(define ,(app pbind sig `(,f [,args : ,_] ...)) : ,rt ,infos ((,ls . ,ts) ...))
     (set! main? (eq? f 'main))
     (set! start (dict-ref infos 'start))
     (set! conclusion (dict-ref infos 'conclusion))
     (define prelude (for/list ([r arg-regs] [a args]) `(movq (reg ,r) ,(arg->opr a))))
     (define body (map (lambda (l t) `(,l . (block () ,@(if (eq? l start) prelude '()) ,@(sel-tail l t))))
                       ls ts))
     `(define ,sig : ,rt ((cfg . ,cfg) ,@infos)
        ,body)]
    ))

(define (select-instructions prog)
  (match prog
    [`(program ,infos ,defs ...)
     `(program ,infos ,@(map sel-def defs))]
    ))
