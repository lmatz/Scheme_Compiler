(define fxshift 2)
(define fxmask #x03)
(define fxtag  #x00)
(define bool_f #x2F)
(define bool_t #x6F)
(define bool_mask #xbf)
(define bool_bit 6)
(define bool_tag  #x2f)
(define wordsize 4)
(define nullval #b00111111)
(define charshift 8)
(define charmask #xff)
(define chartag #b00001111)


(define fixnum-bits (- (* wordsize 8) fxshift))

(define fxlower (- (expt 2 (- fixnum-bits 1))))

(define fxupper (sub1 (expt 2 (- fixnum-bits 1))))

(define (fixnum? x)
  (and (integer? x) (exact? x) (<= fxlower x fxupper)))

(define (immediate? x )
  (or (fixnum? x) (boolean? x) (char? x) (null? x)))

(define  (immediate-rep x) 
  (cond
    ((fixnum? x) (ash x fxshift))
    ((eq? x #t) bool_t )
    ((eq? x #f) bool_f )
    ((char? x) (+ (ash (char->integer x) charshift) chartag ))
    ((null? x) nullval) 
    (else (error "must not happen"))))
    
(define (emit-immediate x)
  (emit "    movl $~s, %eax" (immediate-rep x)))
  
  
;************************if***************************

(define unique-label
  (let ([count 0])
    (lambda ()
      (let ([L (format "L_~s" count)])
        (set! count (add1 count))
        L))))
        
(define (if? expr)
  (and (pair? expr) (eq? (car expr) 'if)))

(define (if-test expr)
  (cadr expr))
  
(define (if-conseq expr)
  (caddr expr))
  
(define (if-altern expr)
  (cadddr expr))

(define (emit-if expr)
  (let ([alt-label (unique-label)]
        [end-label (unique-label)])
      (emit-expr (if-test expr))
      (emit "    cmp $~s, %al" bool-f)
      (emit "    je ~a" alt-label)
      (emit-expr (if-conseq expr))
      (emit "    jmp ~a" end-label)
      (emit "~a:" alt-label)
      (emit-expr (if-altern expr))
      (emit "~a:" end-label))) 

;**********************Binary*************************

(define-syntax define-primitive
  (syntax-rules ()
   [(_ (prim-name si arg* ...) b b* ...)
    (begin
      (putprop ’prim-name ’*is-prim* #t)
      (putprop ’prim-name ’*arg-count*
        (length ’(arg* ...)))
      (putprop ’prim-name ’*emitter*
        (lambda (si arg* ...) b b* ...)))]))
        
(define (emit-primcall si expr)
  (let ((prim (car expr)) (args (cdr expr)))
    (check-primcall-args prim args)
    (apply (primitive-emitter prim) si args)))
    
(define (emit-expr si expr)
  (cond
   [(immediate? expr) (emit-immediate expr)]
   [(if? expr)        (emit-if si expr)]
   [(primcall? expr)  (emit-primcall si expr)]
   [else (error ---)]))
  
;**********************primitive**********************

(define (check-primcall-args prim args)
  (let ((n (getprop prim '*arg-count*))
        (m (length args)))
    (if (= m n)
        #t
      (error "illegal argnum:" m 'for n))))
    
(define-syntax define-primitive
  (syntax-rules ()
   [(_ (prim-name arg* ...) b b* ...)
    (begin
      (putprop 'prim-name '*is-prim* #t)
      (putprop 'prim-name '*arg-count*
        (length '(arg* ...)))
      (putprop 'prim-name '*emitter*
        (lambda (arg* ...) b b* ...)))]))
        
(define (primitive? x)
  (and (symbol? x) (getprop x '*is-prim*)))
  
(define (primcall? expr)
  (and (pair? expr) (primitive? (car expr))))

(define (primitive-emitter x)
  (or (getprop x '*emitter*) (error ---)))

(define (emit-primcall expr)
  (let ([prim (car expr)] [args (cdr expr)])
    (check-primcall-args prim args)
    (apply (primitive-emitter prim) args)))
    
    
(define (emit-expr expr)
  (cond
   [(immediate? expr) (emit-immediate expr)]
   [(if? expr) (emit-if expr)]
   [(and? expr)       (emit-and si expr)]
   [(or? expr)        (emit-or si expr)]
   [(primcall? expr)  (emit-primcall expr)]
   [(predicate-call? expr) (emit-predicate-val si expr)]
   (else (error "not implemented"))))

(define (emit-function-header header)
  (emit "    .section        __TEXT,__text,regular,pure_instructions")
  (emit (string-append "    .global _" header ))
  (emit "    .align  4, 0x90")
  (emit (string-append "_" header ":        ## @scheme_entry"))
  (emit "    .cfi_startproc")
  (emit "## BB#0:"))
  
(define (compile-program expr)
  (emit-function-header "scheme_entry")
  ￼(emit  "movl %esp, %ecx")
  (emit " movl 4(%esp), %esp")
  (emit " call L_scheme_entry")
  (emit " movl %ecx, %esp")
  (emit " ret")
  (emit-label "L_scheme_entry")
  (emit-expr (- wordsize) expr)
  (emit "    ret")
  (emit "    .cfi_endproc"))
  
;********************unary primitive*******************  
  
(define-primitive ($fxadd1 arg)
  (emit-expr arg)
  (emit "    addl $~s, %eax" (immediate-rep 1)))
  
(define-primitive ($fxsub1 arg)
  (emit-expr arg)
  (emit "    subl $~s, %eax" (immediate-rep 1)))
    
(define-primitive ($fixnum->char arg)
  (emit-expr arg)
  (emit "    shll $~s, %eax" (- charshift fxshift))
  (emit "    orl $~s, %eax" chartag)) 
  
(define-primitive ($char->fixnum arg)
  (emit-expr arg)
  (emit "    shrl $~s, %eax" (- charshift fxshift)))
  
(define-primitive ($fxzero? arg)
  (emit-expr arg)
  (emit "    testl %eax, %eax")
  (emit "    sete %al")
  (emit "    movzbl %al, %eax")
  (emit "    sal $~s, %al" bool_bit)
  (emit "    or $~s, %al" bool_f))

(define-primitive (fixnum? arg)
  (emit-expr arg)
  (emit "    and $~s, %al" fxmask)
  (emit "    cmp $~s, %al" fxtag)
  (emit "    sete %al")
  (emit "    movzbl %al, %eax")
  (emit "    sal $~s, %al" bool_bit)
  (emit "    or $~s, %al" bool_f))
  
(define-primitive ($fxzero? arg)
  (emit-expr arg)
  (emit "    testl %eax, %eax")
  (emit "    sete %al")
  (emit "    movzbl %al, %eax")
  (emit "    sal $~s, %al" bool_bit)
  (emit "    or $~s, %al" bool_f))
  
(define-primitive (null? arg)
  (emit-expr arg)
  (emit "    cmp $~s, %al" nullval)
  (emit "    sete %al")
  (emit "    movzbl %al, %eax")
  (emit "    sal $~s, %al" bool_bit)
  (emit "    or $~s, %al" bool_f))
  
(define-primitive (boolean? arg)
  (emit-expr arg)
  (emit "    and $~s, %al" bool_mask)
  (emit "    cmp $~s, %al" bool_tag)
  (emit "    sete %al")
  (emit "    movzbl %al, %eax")
  (emit "    sal $~s, %al" bool_bit)
  (emit "    or $~s, %al" bool_f))

(define-primitive (char? arg)
  (emit-expr arg)
  (emit "    and $~s, %al" charmask)
  (emit "    cmp $~s, %al" chartag)
  (emit "    sete %al")
  (emit "    movzbl %al, %eax")
  (emit "    sal $~s, %al" bool_bit)
  (emit "    or $~s, %al" bool_f))

(define-primitive (not arg)
  (emit-expr arg)
  (emit "    cmp $~s, %al" bool_f)
  (emit "    sete %al")
  (emit "    movzbl %al, %eax")
  (emit "    sal $~s, %al" bool_bit)
  (emit "    or $~s, %al" bool_f))

(define-primitive ($fxlognot arg)
  (emit-expr arg)
  (emit "    notl %eax")
  (emit "    and $~s, %eax" (fxlognot fxmask)))
  
(load "./test-driver.scm")
(load "./tests-1.1-req.scm")
(load "./tests-1.2-req.scm")
(load "./tests-1.3-req.scm")
(load "./tests-1.4-req.scm")
(load "./tests-1.5-req.scm")
