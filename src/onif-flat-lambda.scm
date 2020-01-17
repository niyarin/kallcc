(include "./onif-misc.scm")

(define-library (onif flat-lambda)
   (import (scheme base)
           (scheme cxr)
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

     (define (%flat-conv 
               code 
               lambdas-box 
              offset-box 
              symbol-hash 
              expand-environment)
       (cond 
         ((not (pair? code))
          code)
         ((%lambda-meta-operator? (car code) symbol-hash);<= ONLY body is 1.
            (let* ((id (car offset-box))
                   (_ (set-car! 
                        offset-box
                        (+ id 1)))

                   (new-body (%flat-conv 
                               (cadddr code)
                               lambdas-box
                               offset-box
                               symbol-hash
                               expand-environment))
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
                 (cadr
                  (hash-table-ref
                    symbol-hash
                   'LFUN))
                  id
                  ))))
         (else 
           (map 
             (lambda (x)
               (%flat-conv 
                 x
                 lambdas-box
                 offset-box
                 symbol-hash
                 expand-environment))
             code))
         ))

     (define (onif-flat-flat-code&id-lambdas 
               code
               lambda-id-offset 
               syntax-symbol-hash  
               expand-environment)
       (let* ((lambdas-box (list '()))
              (flat-code (%flat-conv 
                           code 
                           lambdas-box
                           (list lambda-id-offset) 
                           syntax-symbol-hash 
                           expand-environment)))
         (values
           flat-code
            (car lambdas-box))))))
