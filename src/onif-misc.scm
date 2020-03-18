(include "./onif-symbol.scm")

(define-library (onif misc)
   (import (scheme base)
           (srfi 125)
           (scheme cxr)
           (onif symbol))
   (export onif-misc/make-check-onif-symbol-base-function
           onif-misc/lambda-operator?
           onif-misc/begin-operator?
           onif-misc/lambda-meta-operator?
           onif-misc/onif-symbol-hash-ref
           onif-misc/ref-operations
           onif-misc/filter-&-elses
           onif-misc/tail-pair
           onif-misc/tail-pair-res
           onif-misc/tail-pair-push!)
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

     (define onif-misc/begin-operator?
       (onif-misc/make-check-onif-symbol-base-function 'begin))

     (define (onif-misc/onif-symbol-hash-ref onif-symbol-hash symbol)
        (cond
          ((hash-table-ref onif-symbol-hash symbol) => cadr)
          (else #f)))

     (define (onif-misc/ref-operations operator onif-symbol-hash)
       (if (not (onif-symbol? operator))
         #f
         (case (onif-symbol/ref-symbol operator)
            ((CONS CAR CDR PAIR?)
             => (lambda (x) x))
            (else #f))))

     (define (onif-misc/tail-pair)
        (let ((head (cons #f #f)))
          (cons head head)))

     (define onif-misc/tail-pair-res cadar)

     (define (onif-misc/tail-pair-push! tail-pair x)
       (set-cdr! (cdr tail-pair) (list x))
       (set-cdr! tail-pair (cddr tail-pair)))

     (define (onif-misc/filter-&-elses fn ls)
       (let ((res1 (onif-misc/tail-pair))
             (elses (onif-misc/tail-pair)))
          (let loop ((ls ls))
            (cond ((null? ls) (values
                                (onif-misc/tail-pair-res res1)
                                (onif-misc/tail-pair-res elses)))
                  ((fn (car ls))
                   (onif-misc/tail-pair-push! res1 (car ls))
                   (loop (cdr ls)))
                  (else
                   (onif-misc/tail-pair-push! elses (car ls))
                (loop (cdr ls)))))))))
