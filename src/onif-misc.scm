(include "./onif-symbol.scm")

(define-library (onif misc)
   (import (scheme base)
           (srfi 125)
           (onif symbol))
   (export onif-misc/make-check-onif-symbol-base-function
           onif-misc/lambda-operator?
           onif-misc/lambda-meta-operator?
           onif-misc/onif-symbol-hash-ref)
   (begin
     (define (onif-misc/make-check-onif-symbol-base-function target-symbol)
       (lambda (operator onif-symbol-hash)
          (cond
             ((not (onif-symbol? operator)) #f)
             ((eq? (cadr (hash-table-ref onif-symbol-hash target-symbol))
                   operator))
             (else #f))))

     (define onif-misc/lambda-meta-operator?
       (onif-misc/make-check-onif-symbol-base-function 'lambda-META))

     (define onif-misc/lambda-operator?
       (onif-misc/make-check-onif-symbol-base-function 'lambda))

     (define (onif-misc/onif-symbol-hash-ref onif-symbol-hash symbol)
        (cond
          ((hash-table-ref onif-symbol-hash symbol) => cadr)
          (else #f)))
     ))
