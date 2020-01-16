(include "./onif-idebug.scm")

(define-library (onif flat-lambda)
   (import (scheme base)
           (scheme cxr)
           (onif idebug)(scheme write)
           (srfi 125);scheme hash
           (onif symbol))
   (export onif-flat-flat-code&id-lambdas) 
   (begin
     (define (%lambda-operator? operator onif-symbol-hash)
        (cond
          ((not (onif-symbol? operator)) #f)
          ((eq? (cadr (hash-table-ref onif-symbol-hash 'lambda))
                operator))
          (else #f)))
     (define (%lambda-meta-operator? operator onif-symbol-hash)
        (cond
          ((not (onif-symbol? operator)) #f)
          ((eq? (cadr (hash-table-ref onif-symbol-hash 'lambda-META))
                operator))
          (else #f)))
     (define (%if-operator? operator onif-symbol-hash)
        (cond
          ((not (onif-symbol? operator)) #f)
          ((eq? (cadr (hash-table-ref onif-symbol-hash 'if))
                operator))
          (else #f)))


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
