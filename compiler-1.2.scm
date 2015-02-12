(define fxshift 2)
(define fxmask #x03)
(define bool_f #x2F)
(define bool_t #x6F)
(define wordsize 4)
(define nullval #b00111111)
(define charshift 8)
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

(define (compile-program x)
  (unless (immediate? x) (error ---))
  (emit "    .section        __TEXT,__text,regular,pure_instructions")
  (emit "    .global _scheme_entry")
  (emit "    .align  4, 0x90")
  (emit "_scheme_entry:        ## @scheme_entry")
  (emit "    .cfi_startproc")
  (emit "## BB#0:")
  (emit "    movl $~s, %eax" (immediate-rep x))
  (emit "    ret")
  (emit "    .cfi_endproc"))

(load "./test-driver.scm")
(load "./tests-1.1-req.scm")
(load "./tests-1.2-req.scm")

