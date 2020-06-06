(define-library (onif-lib list)
   (import (onif-lib core))
   (export reverse)
   (begin
     ;;simple imprementation
     (define %reverse
       (lambda (input-ls res)
         (if (eq? input-ls '())
           res
           (%reverse (cdr input-ls)
                     (cons (car input-ls) res)))))
     (define reverse (lambda (input-list) (%reverse input-list '())))))
