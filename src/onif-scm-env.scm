(include "./onif-symbol.scm")

(define-library (onif scm env)
   (cond-expand
     ((library (srfi 125))
         (import (scheme base)
                 (srfi 125)
                 (onif symbol)))
     ((library (scheme hash-table))
         (import (scheme base)
                 (scheme hash-table)
                 (onif symbol))))

   (export onif-scm-env-tiny-core
           onif-scm-env/make-env-for-library)

   (begin
     (define %scm-env-tiny-core
       (let ((res #f))
         (lambda ()
           (if res
             res
             (begin
               (set! res
                     `((lambda . (built-in-lambda ,(onif-symbol 'LAMBDA)))
                       (lambda-META . (built-in-lambda-meta ,(onif-symbol 'LAMBDA-META)))
                       (if . (built-in-if ,(onif-symbol 'IF)))
                       (define . (built-in-define  ,(onif-symbol 'DEFINE)))
                       (set! . (built-in-set! ,(onif-symbol 'SET!)))
                       (quote . (built-in-quote ,(onif-symbol 'QUOTE)))
                       (define-syntax . (built-in-set! ,(onif-symbol 'DEFINE-SYNTAX)))
                       (let-syntax . (built-in-set! ,(onif-symbol 'LET-SYNTAX)))
                       (begin . (built-in-begin ,(onif-symbol 'BEGIN)))
                       (define-library . (built-in-define-library ,(onif-symbol 'DEFINE-LIBRARY-SYNTAX)))
                       (import . (built-in-import ,(onif-symbol 'IMPORT-SYNTAX)))
                       (export . (built-in-export ,(onif-symbol 'IMPORT-EXPORT)))

                       (car . (built-in-car ,(onif-symbol 'CAR)))
                       (cdr . (built-in-cdr ,(onif-symbol 'CDR)))
                       (set-car! . (built-in-car ,(onif-symbol 'SET-CAR!)))
                       (set-cdr! . (built-in-car ,(onif-symbol 'SET-CDR!)))
                       (cons . (built-in-cons ,(onif-symbol 'CONS)))
                       (eq? . (built-in-eq? ,(onif-symbol 'EQ?)))
                       (pair? . (built-in-pair? ,(onif-symbol 'PAIR?)))

                       (fx+ . (built-in-fx+ ,(onif-symbol 'FX+)))
                       (fx- . (built-in-fx- ,(onif-symbol 'FX-)))
                       (fx* . (built-in-fx* ,(onif-symbol 'FX*)))
                       (fxquotient . (built-in-fxquotient ,(onif-symbol 'FXQUOTIENT)))
                       (fxremainder . (built-in-fxremainder ,(onif-symbol 'FXREMAINDER)))
                       (fx=? . (built-in-fx=? ,(onif-symbol 'FX=?)))
                       (fx>? . (built-in-fx>? ,(onif-symbol 'FX>?)))
                       (fx<? . (built-in-fx<? ,(onif-symbol 'FX<?)))
                       (fxand . (built-in-fxand ,(onif-symbol 'FXAND)))
                       (fxior . (built-in-fxior ,(onif-symbol 'FXIOR)))
                       (fxnot . (built-in-fxnot ,(onif-symbol 'FXNOT)))

                       (bytevector-u8-ref . (built-in-bytevector-u8-ref ,(onif-symbol 'BYTEVECTOR-U8-REF)))
                       (bytevector-u8-set! . (built-in-bytevector-u8-set! ,(onif-symbol 'BYTEVECTOR-U8-SET!)))
                       (make-bytevector . (built-in-make-bytevector ,(onif-symbol 'MAKE-BYTEVECTOR)))
                       (bytevector-length . (built-in-bytevector-length ,(onif-symbol 'BYTEVECTOR-LENGTH)))

                       (DEFUN . (internal-defun-operator ,(onif-symbol 'DEFUN-INTERNAL)))
                       (LFUN . (internal-lfun-operator ,(onif-symbol 'LFUN-INTERNAL)))))
               res)))))

     (define (onif-scm-env/make-env-for-library)
       (let ((tiny-core (%scm-env-tiny-core)))
         (alist->hash-table
             (map (lambda (symbol)
                    (assq symbol tiny-core))
                  '(import export begin))
             eq?)))

     (define (onif-scm-env-tiny-core)
         (alist->hash-table
           (%scm-env-tiny-core)
           eq?))))
