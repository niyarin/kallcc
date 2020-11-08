(define-library (scmspec core)
   (import (scheme base) (only (scheme list) every))
   (export scmspec/lcheck scmspec/pair scmspec/any? scmspec/list-of
           scmspec/or scmspec/eq scmspec/not scmspec/list)
   (begin
     (define (scmspec/pair car-spec cdr-spec)
       (lambda (x)
         (and (pair? x)
              (apply-spec* car-spec (car x))
              (apply-spec* cdr-spec (cdr x)))))

     (define (scmspec/list . specs)
       (lambda (x)
         (and (list? x)
              (let loop ((specs specs)
                         (input x))
                (cond
                  ((and (null? specs) (null? input)) #t)
                  ((null? specs) #f)
                  ((not (apply-spec* (car specs) (car input))) #f)
                  (else (loop (cdr specs) (cdr input))))))))

     (define (scmspec/list-of spec)
       (lambda (x)
         (and (list? x)
              (every (lambda (i) (apply-spec* spec i))
                      x))))

     (define (scmspec/eq object)
       (lambda (x)
         (eq? x object)))

     (define (scmspec/any? x) #t)

     (define (scmspec/not spec)
       (lambda (x)
         (not (apply-spec* spec x))))

     (define (scmspec/or . specs)
       (lambda (x)
         (let loop ((specs specs))
           (cond
             ((null? specs) #f)
             ((apply-spec* (car specs) x) #t)
             (else (set! **state #f)
                   (loop (cdr specs)))))))


     (define **state #f)
     (define (apply-spec* spec input)
       (let ((res (cond
                     ((procedure? spec) (spec input))
                     ((list? spec) (error "TBW"))
                     ((pair? spec) (error "TBW"))
                     ((vector? spec) (error "TBW"))
                     ((string? spec) (error "TBW")))))
       (if res
         res
         (begin
           (unless **state (set! **state (vector 321 input)))
           #f))))


     (define (apply-spec** spec input)
       (dynamic-wind
         (lambda () (set! **state #f))
         (lambda ()
           (let ((res (apply-spec* spec input)))
             (if res #t **state)))
         (lambda () (set! **state #f))))

     (define-syntax %apply-spec
       (syntax-rules ()
          ((%apply-spec input spec spec-mode)
             (let ((spec-res (apply-spec** spec input)))
               (if (eq? spec-res #t)
                 #t
                 (error "scm spec error." spec-mode (quote spec) (vector 'target (quote input))
                        input (vector spec-res)))))))

     (define-syntax %input-specs
        (syntax-rules ()
           ((_ ()) #t)
           ((_ ((sym spec) rest ...))
            (begin (%apply-spec sym spec "input spec")
                   (%input-specs (rest ...))))))

     (define-syntax %lcheck-aux
         (syntax-rules (input output)
            ((_ () #f bodies ...) (begin bodies ...))
            ((_ () res bodies ...) res)
            ((_ ((input input-spec ...) rest ...) res bodies ...)
             (begin
               (%input-specs (input-spec ...))
               (%lcheck-aux (rest ...) res bodies ...)))
            ((_ ((output output-spec) rest ...) res bodies ...)
             (let ((<result> (begin bodies ...)))
               (%apply-spec <result> output-spec "output spec")
               (%lcheck-aux (rest ...) <result>)))))

     (define-syntax scmspec/lcheck
        (syntax-rules ()
         ((_ specs bodies ...)
          (%lcheck-aux specs #f bodies ...))))))
