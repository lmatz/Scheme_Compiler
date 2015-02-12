(define fxshift 2)
(define fxmask #x03)
(define fxtag  #x00)
(define bool_f #x2F)
(define bool_t #x6F)
(define bool_mask #xbf)
(define bool_bit 6)
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
  
(define (primitive-emitter x)
  (or (getprop x '*emitter*) (error ---)))
  
(define (primcall? expr)
  (and (pair? expr) (primitive? (car expr))))

(define (emit-primcall expr)
  (let ([prim (car expr)] [args (cdr expr)])
    (check-primcall-args prim args)
    (apply (primitive-emitter prim) args)))
    
(define (emit-expr expr)
  (cond
   [(immediate? expr) (emit-immediate expr)]
   [(primcall? expr)  (emit-primcall expr)]
   [else (error ---)]))

(define (emit-function-header header)
  (emit "    .section        __TEXT,__text,regular,pure_instructions")
  (emit (string-append "    .global _" header ))
  (emit "    .align  4, 0x90")
  (emit (string-append "_" header ":        ## @scheme_entry"))
  (emit "    .cfi_startproc")
  (emit "## BB#0:"))

(define (compile-program expr)
  (emit-function-header "scheme_entry")
  (emit-expr expr)
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
