(include "./onif-misc.scm")
(include "./onif-idebug.scm")

(define-library (onif flat-lambda)
   (import (scheme base)
           (scheme cxr)
           (onif idebug)
           (only (scheme list) filter)
           (srfi 125);scheme hash
           (onif misc)
           (onif symbol))
   (export onif-flat-flat-code&id-lambdas)
   (begin
     (define %lambda-operator?
       (onif-misc/make-check-onif-symbol-base-function 'lambda))

     (define %lambda-meta-operator?
       (onif-misc/make-check-onif-symbol-base-function 'lambda-META))

     (define %if-operator?
       (onif-misc/make-check-onif-symbol-base-function 'if))

     (define (%list-set-difference2 ls1 ls2)
       (filter
         (lambda (x) (not (member x ls2)))
         ls1))

     (define (%flat-conv
               code
               lambdas-box
               prev-info
               offset-box
               symbol-hash
               expand-environment)
       (cond
         ((not (pair? code)) code)
         ((%lambda-meta-operator? (car code) symbol-hash);<= ONLY body is 1.
            (let* ((id (car offset-box))
                   (_ (set-car!  offset-box (+ id 1)))
                   (new-body (%flat-conv
                               (cadddr code)
                               lambdas-box
                               (caddr code)
                               offset-box
                               symbol-hash
                               expand-environment))
                   (prev-contain-symbols
                     (cond
                       ((assq 'contain-symbols prev-info)
                        => cadr)
                       (else '())))
                   (contain-symbols
                     (%list-set-difference2
                        (%list-set-difference2
                           prev-contain-symbols
                           (cadr (assq 'contain-symbols (caddr code))))
                        (cadr code)))
                   (new-lambda (list (car code);OPE
                                     (cadr code);FORMALS
                                     (caddr code);META INFO
                                     new-body)))
               (begin
                  (set-car!
                    lambdas-box
                    (cons
                      (list id new-lambda)
                      (car lambdas-box)))
                  (list
                    (onif-misc/onif-symbol-hash-ref symbol-hash 'LFUN)
                    id
                    contain-symbols))))
         (else
           (map
             (lambda (x)
               (%flat-conv
                 x
                 lambdas-box
                 prev-info
                 offset-box
                 symbol-hash
                 expand-environment))
             code))))

     (define (onif-flat-flat-code&id-lambdas
               code
               lambda-id-offset
               syntax-symbol-hash
               expand-environment)
       (let* ((lambdas-box (list '()))
              (prev-info '())
              (flat-code (%flat-conv
                           code
                           lambdas-box
                           prev-info
                           (list lambda-id-offset)
                           syntax-symbol-hash
                           expand-environment)))
         (values
           flat-code
            (car lambdas-box))))))
