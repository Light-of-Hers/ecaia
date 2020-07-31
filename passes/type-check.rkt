#lang racket

(require "utils.rkt")

(provide type-check type-tag)

(define arity-table (make-hash))
(for ([op '(+ -)]) (hash-set! arity-table op `((Integer Integer) Integer)))
(for ([op '(< > <= >= eq?)]) (hash-set! arity-table op `((Integer Integer) Boolean)))
(for ([op '(and or)]) (hash-set! arity-table op `((Boolean Boolean) Boolean)))

(define (tce e t) (values `(has-type ,e ,t) t))
(define (type-check-exp env e)
  (define recur ((curry type-check-exp) env))    
  (match e
    [`(has-type ,_ ,t) (values e t)]
    [(? fixnum?) (tce e `Integer)]
    [(? boolean?) (tce e `Boolean)]
    [(? symbol?) (tce e (dict-ref env e))]
    [`(void) (tce e `Void)]
    [`(read) (tce e `Integer)]
    [`(allocate ,_ ,t) (tce e t)]
    [`(global-value ,_) (tce e `Integer)]
    [`(collect ,_) (tce e `Void)]
    [`(let ([,x ,e]) ,body)
     (define-values (e^ t) (recur e))
     (when (and (eq? x '_) (not (equal? t `Void)))
       (error 'type-check-exp "must bind a void to _" e))
     (define-values (b^ t-b) (type-check-exp (if (eq? x '_) env `((,x . ,t) ,@env)) body))
     (tce `(let ([,x ,e^]) ,b^) t-b)]
    [`(if ,cnd ,thn ,els)
     (define-values (cn ct) (recur cnd))
     (define-values (th tt) (recur thn))
     (define-values (el et) (recur els))
     (unless (equal? ct `Boolean)
       (error `type-check-exp "condition must be a boolean, not ~a" ct))
     (unless (equal? tt et)
       (error `type-check-exp "two branches must have same type ~a : ~a" tt et))
     (tce `(if ,cn ,th, el) tt)]
    [`(vector ,es ...)
     (define-values (e* t*) (for/lists (_e* _t*) ([e es]) (recur e)))
     (let ([t `(Vector ,@t*)])
       (tce `(vector ,@e*) t))]
    [`(vector-ref ,e ,i)
     (define-values (e^ t) (recur e))
     (match t
       [`(Vector ,ts ...)
        (unless (and (exact-nonnegative-integer? i) (< i (length ts)))
          (error `type-check-exp "invalid␣index␣~a" i))
        (let ([t (list-ref ts i)])
          (tce `(vector-ref ,e^ ,i) t))]
       [else (error `type-check-exp "expected␣a␣vector␣in␣vector-ref,␣not ~a" t)]
       )]
    [`(vector-set! ,e ,i ,e1)
     (define-values (e^ t) (recur e))
     (define-values (e1^ t1) (recur e1))
     (match t
       [`(Vector ,ts ...)
        (unless (and (exact-nonnegative-integer? i) (< i (length ts)))
          (error `type-check-exp "invalid index ~a" i))
        (let ([t (list-ref ts i)])
          (unless (equal? t t1)
            (error `type-check-exp "expected ~a in vector-set!, not ~a" t t1))
          (tce `(vector-set! ,e^ ,i ,e1^) `Void))]
       [else (error `type-check-exp "expected a vector in vector-set!, not ~a" t)]
       )]
    [`(lambda: ,(app pbind sig `([,xs : ,ts] ...)) : ,rt ,body)
     (when (< (set-count (apply set xs)) (length xs))
       (error 'type-check-exp "re-define arguments" xs))
     (define ext-env (for/fold ([env env])
                               ([x xs] [t ts])
                       (cons `(,x . ,t) env)))
     (define-values (b bt) (type-check-exp ext-env body))
     (unless (equal? bt rt)
       (error 'type-check-exp "expect return type" rt))
     (tce `(lambda: ,sig : ,rt ,b) `(,@ts -> ,rt))]
    [`(,op ,es ...)
     (define-values (op^ arity) (cond
                                  [(and (eq? op '-) (= (length es) 1)) (values op `((Integer) Integer))]
                                  [(and (eq? op 'not) (= (length es) 1) (values op `((Boolean) Boolean)))]
                                  [(builtin-op? op) (values op (hash-ref arity-table op))]
                                  [else (let-values ([(op t) (recur op)])
                                          (match t
                                            [`(,ats ... -> ,rt) (values op `(,ats ,rt))]
                                            ))]
                                  ))
     (define-values (e* t*) (for/lists (_0 _1) ([e es]) (recur e)))
     (define (check arg-types ret-type)
       (unless (equal? t* arg-types)
         (error `type-check-exp "fuck your mother" t*))
       (tce `(,op^ ,@e*) ret-type))
     (apply check arity)]
    ))

(define (type-tag e)
  (define-values (e^ _) (type-check-exp '() e))
  e^)

(define (collect-def-types defs)
  (foldr (lambda (def env)
           (match def
             [`(define (,f (,_ : ,ts) ...) : ,rt ,_)
              (when (dict-has-key? env f)
                (error 'collect-def-types "re-define" f))
              (when (builtin-op? f)
                (error 'collect-def-types "overwirte builtin-op" f))
              `((,f . (,@ts -> ,rt)) ,@env)]
             ))
         '() defs))

(define (type-check-def env def)
  (match def
    [`(define ,(app pbind sig `(,_ [,args : ,ts] ...)) : ,rt ,body)
     (when (< (set-count (apply set args)) (length args))
       (error 'type-check-def "re-define arguments" args))
     (let ([env (foldr (lambda (arg t env) `((,arg . ,t) ,@env)) env args ts)])
       (define-values (b bt) (type-check-exp env body))
       (unless (equal? bt rt)
         (error 'type-check-def "expect return type" rt))
       `(define ,sig : ,rt ,b))]
    ))

(define (type-check e)
  (match e
    [`(program ,infos ,defs ... ,body)
     (define env (collect-def-types defs))
     (define ds  (map ((curry type-check-def) env) defs))
     (define-values (b t) (type-check-exp env body))
     (unless (equal? t `Integer)
       (error 'type-check "program's type must be integer" t))
     `(program ,infos ,@ds ,b)]
    ))
