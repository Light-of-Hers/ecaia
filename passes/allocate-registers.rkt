#lang racket

(require "./utils.rkt")
(require graph)

(provide allocate-registers)

(define (color-graph g)
  (define nodes (get-vertices g))
  (define node->color (make-hash))
  (define node->sat (make-hash))
  (define (set-color! nd c)
    (hash-set! node->color nd c)
    (for ([v (get-neighbors g nd)])
      (let ([sat (hash-ref! node->sat v mutable-set)])
        (set-add! sat c))))
  (define W '())
  (for ([nd nodes])
    (match nd
      [`(var ,_) (set! W (cons nd W))]
      [`(reg ,r) (set-color! nd (register->color r))]))
  (define (find-color u)
    (define res 0)
    (define sat (hash-ref! node->sat u mutable-set))
    (while (set-member? sat res)
      (set! res (+ res 1)))
    res)
  (while (not (null? W))
    (set! W (sort W (lambda (a b) (apply > (for/list ([v '(,a ,b)])
                                             (set-count (hash-ref! node->sat v mutable-set)))))))
    (let ([u (car W)])
      (set-color! u (find-color u)))
    (set! W (cdr W)))
  node->color)

(define (Vector? t)
  (match t
    [`(Vector ,_ ...) #t]
    [else #f]
    ))

(define (allocate-registers prog)
  (define node->color '())
  (define locals '())
  (define (ar-def def)
    (define root-depth 0)
    (define color->root-depth (make-hash))
    (define depth 0)
    (define color->depth (make-hash))
    (define sainted (mutable-set))
    (define (opr x)
      (match x
        [`(var ,v)
         (let ([clr (hash-ref node->color x)])
           (if (< clr (num-registers-for-alloc))
               (let ([r (color->register clr)])
                 (when (set-member? callee-save r)
                   (set-add! sainted r))
                 `(reg ,r))
               (if (Vector? (hash-ref locals v))
                   `(deref r15 ,(- (hash-ref! color->root-depth clr (lambda () (set! root-depth (+ root-depth 8)) root-depth))))
                   `(deref rbp ,(- (hash-ref! color->depth clr (lambda () (set! depth (+ depth 8)) depth)))))))]
        [else x]))
    (define (color-inst inst)
      (match inst
        [`(,opt ,oprs ...) `(,opt ,@(map opr oprs))]))
    (match def
      [`(define ,sig : ,rt ,infos ((,ls . (block ,bis ,instss ...)) ...))
       (set! instss (map (lambda (insts) (map color-inst insts)) instss))
       `(define ,sig : ,rt ((space . ,depth)
                            (root-space . ,root-depth)
                            (sainted . ,(set->list sainted))
                            ,@infos)
          ,(for/list ([l ls] [bi bis] [insts instss])
             `(,l . (block ,bi ,@insts))))]
      ))
  (match prog
    [`(program ,infos ,defs ...)
     (let* ([g (dict-ref infos 'conflicts)]
            [lcs (dict-ref infos 'locals)])
       (set! locals lcs)
       (set! node->color (color-graph g))
       `(program ,infos ,@(map ar-def defs)))]
    ))
