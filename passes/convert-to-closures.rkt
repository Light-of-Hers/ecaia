#lang racket

(require "./utils.rkt")
(require "./type-check.rkt")

(provide convert-to-closures)

(define-custom-set-types pair-set
  #:elem? pair?
  (lambda (a b recur-equal?) (recur-equal? (car a) (car b)))
  )

(define (pset . elems)
  (make-immutable-pair-set elems))

(define (cc-type t)
  (match t
    [`(,ts ... -> ,rt) `(Vector (_ ,@ts -> ,rt))]
    [else t]
    ))

(define (free-vars e [t '()])
  (match e
    [`(has-type ,e ,t) (free-vars e t)]
    [(? symbol?) (pset `(,e . ,(cc-type t)))]
    [(? simple?) (pset)]
    [`(let ([,x ,e]) ,body)
     (set-union (free-vars e)
                (set-subtract (free-vars body) (pset `(,x . _))))]
    [`(lambda: ([,as . ,_] ...) : ,_ ,body)
     (set-subtract (free-vars body) (apply pset (for/list ([a as]) `(,a . _))))]
    [`(fun-ref ,_) (pset)]
    [`(,_ ,es ...) (apply set-union (map free-vars es))]
    [else (pset)]
    ))

(define (convert-to-closures prog)
  (define lam-defs '())
  (define (cc-exp env e [t '()])
    (define recur ((curry cc-exp) env))
    (match e
      [`(has-type ,e ,t) (type-tag (recur e t))]
      [(? symbol?) (cond [(assoc e env) => (lambda (et) `(has-type ,e ,(cdr et)))]
                         [else `(has-type ,e ,(cc-type t))]
                         )]
      [(? simple?) `(has-type ,e ,(cc-type t))]
      ;; vector operations
      [`(vector-ref ,e ,i) `(vector-ref ,(recur e) ,i)]
      [`(vector-set! ,e ,i ,v) `(vector-set! ,(recur e) ,i ,(recur v))]
      ;; function reference
      [`(fun-ref ,f)
       `(vector (has-type ,e ,(match t [`(,ts ... -> ,rt) `(_ ,@ts -> ,rt)])))]
      ;; application
      [`(call ,f ,es ...)
       (define tmp (gen-sym "tmp"))
       (define tagged-f (recur f))
       (define tagged-tmp `(has-type ,tmp ,(get-type tagged-f)))
       `(let ([,tmp ,tagged-f])
          (has-type
           (call (vector-ref ,tagged-tmp 0) ,tagged-tmp ,@(map recur es))
           ,(cc-type t)))]
      [`(let ([,x ,e]) ,body)
       (define new-e (recur e))
       (define e-type (get-type new-e))
       (define new-env (cons `(,x . ,e-type) env))
       `(let ([,x ,new-e]) ,(cc-exp new-env body))]
      ;; lambda
      [`(lambda: ([,as : ,ts] ...) : ,rt ,body)
       (define fvts (set->list (free-vars e)))
       (define fvs (map car fvts)) (define fts (map cdr fvts))
       (define new-ts (map cc-type ts)) (define new-rt (cc-type rt))
       (define new-env (for/fold ([env env]) ([a as] [t new-ts])
                         (cons `(,a . ,t) env)))
       (define new-body (cc-exp new-env body))
       (define lam-name (gen-sym "lam"))
       (define lam-type `(_ ,@new-ts -> ,new-rt))
       (define lam-ref `(has-type (fun-ref ,lam-name) ,lam-type))
       (define clos-arg (gen-sym "clos"))
       (define clos-type `(Vector _ ,@fts))
       (define tagged-clos-arg `(has-type ,clos-arg ,clos-type))
       (define def-body (for/foldr ([body new-body])
                          ([fv fvs] [i (in-naturals 1)])
                          `(let ([,fv (vector-ref ,tagged-clos-arg ,i)])
                             ,body)))
       (define new-def `(define (,lam-name [,clos-arg : ,clos-type]
                                           ,@(for/list ([a as] [t new-ts]) `[,a : ,t]))
                          : ,new-rt () ,(type-tag def-body)))
       (set! lam-defs (cons new-def lam-defs))
       `(vector ,lam-ref ,@(for/list ([fv fvs] [ft fts]) `(has-type ,fv ,ft)))]
      ;; other operations
      [`(,op ,es ...)
       `(,op ,@(map recur es))]
      [else `(has-type ,e ,(cc-type t))]
      ))
  (define (cc-def def)
    (match def
      [`(define (,f [,as : ,ts] ...) : ,rt ,infos ,body)
       (define new-ts (map cc-type ts)) (define new-rt (cc-type rt))
       (define clos-arg (gen-sym "clos"))
       (define env (for/fold ([env '()]) ([a as] [t new-ts])
                     (cons `(,a . ,t) env)))
       (define new-body (cc-exp env body))
       `(define (,f [,clos-arg : (Vector _)]
                    ,@(for/list ([a as] [t new-ts]) `[,a : ,t]))
          : ,new-rt ,infos ,new-body)]
      ))
  (match prog
    [`(program ,infos ,defs ...)
     (set! defs (map cc-def defs))
     `(program ,infos ,@lam-defs ,@defs)]
    ))
