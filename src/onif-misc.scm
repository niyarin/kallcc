(include "./onif-symbol.scm")
(include "./onif-idebug.scm")

(define-library (onif misc)
   (import (scheme base)
           (scheme list)
           (srfi 125)
           (scheme write)
           (onif idebug);
           (onif symbol))
   (export onif-misc/make-check-onif-symbol-base-function
           onif-misc/lambda-operator?
           onif-misc/begin-operator?
           onif-misc/lambda-meta-operator?
           onif-misc/define-operator?
           onif-misc/set!-operator?
           onif-misc/if-operator?
           onif-misc/local-set!-operator?
           onif-misc/ref-var-operator?
           onif-misc/quote-operator?
           onif-misc/onif-symbol-hash-ref
           onif-misc/ref-operations
           onif-misc/ft-pair
           onif-misc/ft-pair-res
           onif-misc/ft-pair-push!
           onif-misc/for-each-cell1
           onif-misc/map-indexed
           onif-misc/var?
           onif-misc/const?
           onif-misc/namespace-assq)
   (begin
     (define (onif-misc/var? obj)
       (or (symbol? obj)
           (onif-symbol/onif-symbol? obj)))

     (define (onif-misc/const? obj)
       (and (not-pair? obj)
            (not (onif-misc/var? obj))))

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

     (define onif-misc/quote-operator?
       (onif-misc/make-check-onif-symbol-base-function 'quote))

     (define onif-misc/define-syntax?
       (onif-misc/make-check-onif-symbol-base-function 'define-syntax))

     (define onif-misc/set!-operator?
       (onif-misc/make-check-onif-symbol-base-function 'set!))

     (define onif-misc/local-set!-operator?
       (onif-misc/make-check-onif-symbol-base-function 'local-set!))

     (define onif-misc/ref-var-operator?
       (onif-misc/make-check-onif-symbol-base-function 'ref-var))

     (define (onif-misc/onif-symbol-hash-ref onif-symbol-hash symbol)
        (cond
          ((hash-table-ref onif-symbol-hash symbol) => cadr)
          (else #f)))

     (define (onif-misc/ref-operations operator onif-symbol-hash)
       (if (not (onif-symbol? operator))
         #f
         (case (onif-symbol/ref-symbol operator)
            ((CONS CAR CDR PAIR? SET-CAR! SET-CDR! FX+ FX- FX* FX=? FX<? EQ? FXREMAINDER
              MAKE-VECTOR VECTOR-SET!
              BYTEVECTOR-U8-REF BYTEVECTOR-U8-SET! MAKE-BYTEVECTOR BYTEVECTOR-LENGTH)
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

    (define (onif-misc/map-indexed f ls)
      (let ((res-cell (onif-misc/ft-pair)))
        (let loop ((ls ls)
                   (index 0))
          (if (null? ls)
            (onif-misc/ft-pair-res res-cell)
            (begin
              (onif-misc/ft-pair-push! res-cell (f index (car ls)))
              (loop (cdr ls) (+ index 1)))))))

    (define (onif-misc/namespace-assq  key namespace . default)
        (cond
          ((not namespace) (error "Namespace is null"));;default?
          ((assq key (cadr namespace)) => cadr)
          ((not (null? default)) (car default))
          (else (error "Namespace doesn't have key."
                       key namespace))))))
