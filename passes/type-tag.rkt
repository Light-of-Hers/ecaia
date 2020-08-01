#lang racket

(require "./utils.rkt")

(provide type-tag)

(define (type-tag-exp env e)
  (define recur ((curry type-tag-exp) env))
  (define (tag-it t [e e]) `(has-type ,e ,t))
  (match e
    [`(has-type ,_ ,_) e]
    [`(allocate ,_ ,t) (tag-it t)]
    [(or (? fixnum?) `(read) `(global-value ,_))
     (tag-it `Integer)]
    [(or `(void) `(collect ,_))
     (tag-it `Void)]
    [(? boolean?) (tag-it `Boolean)]
    [(? symbol?) (tag-it (dict-ref env e))]
    [`(fun-ref ,f) (tag-it (dict-ref env f))]

    [`(let ([,x ,e]) ,body)
     (let* ([e (recur e)]
            [env (cons `(,x . ,(get-type e)) env)]
            [body (type-tag-exp env body)])
       (tag-it (get-type body)
               `(let ([,x ,e]) ,body)))]
    [`(if ,cnd ,thn ,els)
     (let* ([cnd (recur cnd)]
            [thn (recur thn)]
            [els (recur els)])
       (tag-it (get-type thn)
               `(if ,cnd ,thn ,els)))]

    [`(vector ,es ...)
     (let* ([es (map recur es)]
            [ts (map get-type es)])
       (tag-it `(Vector ,@ts)
               `(vector ,@es)))]
    [`(vector-ref ,e ,i)
     (let* ([e (recur e)])
       (match (get-type e)
         [`(Vector ,ts ...)
          (tag-it (list-ref ts i)
                  `(vector-ref ,e ,i))]
         ))]
    [`(vector-set! ,e1 ,i ,e2)
     (let* ([e1 (recur e1)]
            [e2 (recur e2)])
       (tag-it `Void
               `(vector-set! ,e1 ,i ,e2)))]

    [`(lambda: ,(app pbind sig `([,as : ,ts] ...)) : ,rt ,body)
     (let* ([env (for/fold ([env env]) ([a as] [t ts])
                   (cons `(,a . ,t) env))]
            [body (type-tag-exp env body)])
       (tag-it `(,@ts -> ,rt)
               `(lambda: ,sig : ,rt ,body)))]

    [`(call ,f ,es ...)
     (let* ([f (recur f)]
            [es (map recur es)])
       (match (get-type f)
         [`(,_ ... -> ,rt)
          (tag-it rt
                  `(call ,f ,@es))]
         ))]

    [`(,(? builtin-op? op) ,es ...)
     (let* ([es (map recur es)]
            [res `(,op ,@es)])
       (match op
         [(or '+ '-) (tag-it `Integer res)]
         [(or 'and 'or 'not
              '< '> 'eq? '<= '>=) (tag-it `Boolean res)]
         ))]
    ))

(define (type-tag e)
  (type-tag-exp '() e))
