(define-library (scmspec core)
   (import (scheme base))
   (export scmspec/lcheck scmspec/pair scmspec/any?)
   (begin
     (define (scmspec/pair car-spec cdr-spec)
       (lambda (x)
         (and (pair? x)
              (car-spec (car x))
              (cdr-spec (cdr x)))))

     (define (scmspec/any? x) #t)

     (define-syntax %apply-spec
       (syntax-rules ()
          ((%apply-spec input spec spec-mode)
             (let ((spec-res (spec input)))
               (if spec-res
                 #t
              (error "scm spec error." spec-mode (quote spec) (quote input)))))))

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
            ((_ ((input input-spec) rest ...) res bodies ...)
             (begin
               (%input-specs input-spec)
               (%lcheck-aux (rest ...) res bodies ...)))
            ((_ ((output output-spec) rest ...) res bodies ...)
             (let ((<result> (begin bodies ...)))
               (%apply-spec <result> output-spec "output spec")
               (%lcheck-aux (rest ...) <result>)))))

     (define-syntax scmspec/lcheck
        (syntax-rules ()
         ((_ specs bodies ...)
          (%lcheck-aux specs #f bodies ...))))))
