#lang racket

(provide pe-arith)

(define (pe-neg r)
  (define (exp-neg e)
    (match e
      [`(read) `(- (read))]
      [`(- (read)) `(read)]
      [`(+ ,(app exp-neg e1) ,(app exp-neg e2))
       `(+ ,e1 ,e2)])) 
  (match r
    [(? fixnum?) (- r)]
    [`(+ ,(? fixnum? n) ,e)
     `(+ ,(- n) ,(exp-neg e))]
    [e (exp-neg e)]
    ))

(define (pe-add r1 r2)
  (define (order r)
    (match r
      [(? fixnum?) 0]
      [`(+ ,(? fixnum?) ,_) 1]
      [else 2]))
  (define reordered
    (if ((order r1) . > . (order r2))
        `(,r2 ,r1)
        `(,r1 ,r2)))
  (match reordered
    [`(,(? fixnum? n1) ,(? fixnum? n2))
     (+ n1 n2)]
    [`(,(? fixnum? n1) (+ ,(? fixnum? n2) ,e2))
     `(+ ,(+ n1 n2) ,e2)]
    [`(,(? fixnum?) ,_)
     `(+ ,r1 ,r2)]
    [`((+ ,(? fixnum? n1) ,e1) (+ ,(? fixnum? n2) ,e2))
     `(+ ,(+ n1 n2) (+ ,e1 ,e2))]
    [`((+ ,(? fixnum? n1) ,e1) ,e2)
     `(+ ,n1 (+ ,e1 ,e2))]
    ))

(define (pe-arith e)
  (match e
    [(? fixnum?) e]
    [`(read) e]
    [`(- ,(app pe-arith r))
     (pe-neg r)]
    [`(+ ,(app pe-arith r1) ,(app pe-arith r2))
     (pe-add r1 r2)]
    [`(,op ,es ...) `(,op ,@(map pe-arith es))]
    [else e]
    ))
