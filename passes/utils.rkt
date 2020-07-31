#lang racket

(provide arg? arg->opr pbind builtin-op?
         simple? gen-sym strip-has-type
         while fix
         general-registers num-registers-for-alloc caller-save callee-save
	 arg-registers rootstack-reg register->color color->register
         registers align byte-reg->full-reg
         fun-call? indirect-call? get-type)

(define (arg? x)
  (match x
    [(? symbol?) #t]
    [(? fixnum?) #t]
    [(? boolean?) #t]
    [`(global-value ,_) #t]
    [else #f]
    ))

(define (simple? e)
  (match e
    [`(global-value ,_) #t]
    [`(allocate ,_ ...) #t]
    [`(collect ,_ ...) #t]
    [(? arg?) #t]
    [else #f]
    ))

(define (arg->opr x)
  (match x
    [(? symbol?) `(var ,x)]
    [(? fixnum?) `(int ,x)]
    [(? boolean?) `(int ,(if x 1 0))]
    [`(global-value ,_) x]
    ))

(define (get-type e)
  (match e
    [`(has-type ,_ ,t) t]
    ))

(define (pbind x) (values x x))

(define builtin-ops (set '+ '- '< '> 'eq? '<= '>= 'and 'or 'not 'read 'if
                         'vector 'vector-ref 'vector-set! 'void 'collect 'allocate 'global-value
                         'fun-ref 'call 'tailcall))
(define (builtin-op? op)
  (set-member? builtin-ops op))

(define (gen-sym s)
  (gensym (string-replace s "-" "_")))

(define (strip-has-type e)
  (match e
    [`(has-type ,e ,_) (strip-has-type e)]
    [(cons a b) (cons (strip-has-type a) (strip-has-type b))]
    [else e]
    ))

(define-syntax-rule (while condition body ...)
  (let loop ()
    (when condition
      body ...
      (loop))))

(define fix (lambda (f) (lambda (x) ((f (fix f)) x))))


(define (fun-call? s)
  (or (eq? s 'call) (eq? s 'tailcall)))

(define (indirect-call? s)
  (or (symbol=? s 'indirect-callq)
      (symbol=? s 'tail-jmp)))

;; System V Application Binary Interface
;; AMD64 Architecture Processor Supplement
;; Edited by Jan Hubicˇka, Andreas Jaeger, Mark Mitchell
;; December 2, 2003

;; We reserve rax and r11 for patching instructions.
;; We reserve r15 for the rootstack pointer. 
(define rootstack-reg 'r15)
;; There are 11 other general registers
;; The ordering here indicates preference in the register allocator.
;; We put the caller-saved registers first.
(define general-registers (vector 'rcx 'rdx 'rsi 'rdi 'r8 'r9 'r10
                                  'rbx  'r12 'r13 'r14))

(define arg-registers (void))
(define registers-for-alloc (void))

;; registers-for-alloc should always inlcude the arg-registers.
(define (use-minimal-set-of-registers! f)
  (if f
      (begin
        ;; need at least 2 arg-registers, see limit-functions -Jeremy
        ;(set! arg-registers (vector 'rcx 'rdx))
        (set! arg-registers (vector 'rcx 'rdx))
        ;(set! registers-for-alloc (vector 'rcx 'rdx)))
        (set! registers-for-alloc (vector 'rbx 'rcx 'rdx)))
      (begin
        (set! arg-registers (vector 'rcx 'rdx 'rdi 'rsi 'r8 'r9))
        (set! registers-for-alloc general-registers))))

(use-minimal-set-of-registers! #f)

;; We don't need to include the reserved registers
;; in the list of caller or callee save registers.
(define caller-save (set 'rcx 'rdx 'rsi 'rdi 'r8 'r9 'r10))
(define callee-save (set 'rbx 'r12 'r13 'r14))

(define byte-register-table
  (make-immutable-hash
   `((ah . rax) (al . rax)
     (bh . rbx) (bl . rbx)
     (ch . rcx) (cl . rcx)
     (dh . rdx) (dl . rdx))))

(define (byte-reg->full-reg x)
  (let ([r? (hash-ref byte-register-table x #f)])
    (unless r?
      (error 'byte-reg->full-reg "invalid byte register ~a" x))
    r?))

(define reg-colors
  '((rax . -1) (r11 . -2) (r15 . -3) (rbp . -4) (__flag . -5)))

(for ([r registers-for-alloc]
      [i (in-naturals)])
  (set! reg-colors (cons (cons r i) reg-colors)))

(define (register->color r)
  (cond [(assq r reg-colors) => (lambda (p) (cdr p))]
        [else -1])) ;; for registers not used in register allocator.
  
;;  (cdr (assq r reg-colors)))

(define (num-registers-for-alloc)
  (vector-length registers-for-alloc))

(define (color->register c)
  (vector-ref registers-for-alloc c))

(define registers (set-union (list->set (vector->list general-registers))
			     (set 'rax 'r11 'r15 'rsp 'rbp '__flag)))

(define (align n alignment)
  (cond [(eq? 0 (modulo n alignment))
	 n]
	[else
	 (+ n (- alignment (modulo n alignment)))]))
