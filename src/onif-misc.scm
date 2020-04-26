(include "./onif-symbol.scm")
(include "./onif-idebug.scm")

(define-library (onif misc)
   (import (scheme base)
           (srfi 125)
           (scheme write)
           (onif idebug);
           (onif symbol))
   (export onif-misc/make-check-onif-symbol-base-function
           onif-misc/lambda-operator?
           onif-misc/begin-operator?
           onif-misc/lambda-meta-operator?
           onif-misc/define-operator?
           onif-misc/if-operator?
           onif-misc/onif-symbol-hash-ref
           onif-misc/ref-operations
           onif-misc/filter-&-elses
           onif-misc/ft-pair
           onif-misc/ft-pair-res
           onif-misc/ft-pair-push!
           onif-misc/for-each-cell1
           onif-misc/map-indexed)
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

     (define onif-misc/define-operator?
       (onif-misc/make-check-onif-symbol-base-function 'define))

     (define onif-misc/if-operator?
       (onif-misc/make-check-onif-symbol-base-function 'if))

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

     (define (onif-misc/ft-pair)
        (let ((head (cons #f '())))
          (cons head head)))

     (define onif-misc/ft-pair-res cdar)

     (define (onif-misc/ft-pair-push! ft-pair x)
       (set-cdr! (cdr ft-pair) (list x))
       (set-cdr! ft-pair (cddr ft-pair)))

     (define (onif-misc/for-each-cell1 fn ls)
       (let loop ((cell ls))
         (when (pair? cell)
            (fn cell)
            (loop (cdr cell)))))

     (define (onif-misc/filter-&-elses fn ls)
       (let ((res1 (onif-misc/ft-pair))
             (elses (onif-misc/ft-pair)))
          (let loop ((ls ls))
            (cond ((null? ls)
                   (values
                       (onif-misc/ft-pair-res res1)
                       (onif-misc/ft-pair-res elses)))
                  ((fn (car ls))
                   (onif-misc/ft-pair-push! res1 (car ls))
                   (loop (cdr ls)))
                  (else
                   (onif-misc/ft-pair-push! elses (car ls))
                   (loop (cdr ls)))))))

    (define (onif-misc/map-indexed f ls)
      (let ((res-cell (onif-misc/ft-pair)))
        (let loop ((ls ls)
                   (index 0))
          (if (null? ls)
            (onif-misc/ft-pair-res res-cell)
            (begin
              (onif-misc/ft-pair-push! res-cell (f index (car ls)))
              (loop (cdr ls) (+ index 1)))))))))
