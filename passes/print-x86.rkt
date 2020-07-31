#lang racket

(require "./utils.rkt")

(provide print-x86)

(define (px-opr x)
  (match x
    [`(int ,i) (format "$~a" i)]
    [`(reg ,r) (format "%~a" r)]
    [`(byte-reg ,r) (format "%~a" r)]
    [`(deref ,r ,i) (format "~a(%~a)" i r)]
    [`(global-value ,gv) (format "~a(%rip)" gv)]
    [`(fun-ref ,f) (format "~a(%rip)" f)]
    [else (format "~a" x)]))

(define (px-inst epi inst)
  (match inst
    [`(set ,cc ,d) (format "\tset~a ~a" cc (px-opr d))]
    [`(jmp-if ,cc ,l) (format "\tj~a ~a" cc l)]
    [`(tail-jmp ,d) (string-join `(,@epi ,(format "\tjmp *~a" (px-opr d))) "\n")]
    [`(indirect-callq ,d) (format "\tcallq *~a" (px-opr d))]
    [`(,opt ,oprs ...) (format "\t~a ~a" opt (string-join (map px-opr oprs) ", "))]
    ))

(define (px-blk l insts epi)
  (string-join `(,(format "~a:" l) ,@(map ((curry px-inst) epi) insts)) "\n"))

(define (px-def def)
  (match def
    [`(define (,f ,_ ...) : ,_ ,infos ((,ls . (block ,_ ,instss ...)) ...))
     (let* ([space (dict-ref infos 'space)]
            [sainted (dict-ref infos 'sainted)]
            [start (dict-ref infos 'start)]
            [conclusion (dict-ref infos 'conclusion)]
            [saved (dict-ref infos 'sainted)]
            [other-space (* 8 (+ 2 (length saved)))]
            [total-space (align (+ other-space space) 16)]
            [space (- total-space other-space)]
            [root-space (dict-ref infos 'root-space)]
            [prologue `(
                        ,@(for/list ([r sainted]) (format "\tpushq %~a" r))
                        "\tpushq %rbp"
                        "\tmovq %rsp, %rbp"
                        ,(format "\tsubq $~a, %rsp" space)
                        ,@(for/list ([off (range 0 root-space 8)]) (format "\tmovq $0, ~a(%r15)" off))
                        ,(format "\taddq $~a, %r15" root-space)
                        )]
            [epilogue `(
                        ,(format "\tsubq $~a, %r15" root-space)
                        ,(format "\taddq $~a, %rsp" space)
                        "\tpopq %rbp"
                        ,@(for/list ([r (reverse sainted)]) (format "\tpopq %~a" r))
                        )])
       (string-join
        `(,@(if (eq? f 'main)
                `(".globl main"
                  ".align 16")
                '())
          ,(format "~a:" f)
          ,@(if (eq? f 'main)
                `("\tmovq $16384, %rdi"
                  "\tmovq $16, %rsi"
                  "\tcallq initialize"
                  "\tmovq rootstack_begin(%rip), %r15")
                '())
          ,@prologue
          ,(format "\tjmp ~a" start)
          ,@(for/list ([l ls] [insts instss])
              (px-blk l insts epilogue))
          ,(format "~a:" conclusion)
          ,@(if (eq? f 'main)
                `("\tmovq %rax, %rdi"
                  "\tcallq print_int")
                '())
          ,@epilogue
          "\tretq"
          ) "\n"))]
    ))

(define (print-x86 prog)
  (match prog
    [`(program ,_ ,defs ...)
     (string-join (map px-def defs) "\n" #:after-last "\n")]
    ))
