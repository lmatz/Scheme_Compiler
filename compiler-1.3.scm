(define-syntax define-primitive
  (syntax-rules ()
   [(_ (prim-name arg* ...) b b* ...)
    (begin
      (putprop ’prim-name ’*is-prim* #t)
      (putprop ’prim-name ’*arg-count*
        (length ’(arg* ...)))
      (putprop ’prim-name ’*emitter*
        (lambda (arg* ...) b b* ...)))]))
(define (primitive? x)
  (and (symbol? x) (getprop x ’*is-prim*)))
(define (primitive-emitter x)
  (or (getprop x ’*emitter*) (error ---)))
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
(define (emit-program expr)
  (emit-function-header "scheme_entry")
  (emit-expr expr)
  (emit "    ret"))
