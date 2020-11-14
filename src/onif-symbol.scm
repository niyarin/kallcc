(define-library (onif symbol)
   (import (scheme base)
           (scheme case-lambda))
   (export onif-symbol onif-symbol? onif-symbol->symbol
           onif-symbol/onif-symbol?  onif-symbol/ref-symbol)
   (begin
     (define *id-counter* 0)
     (define-record-type <onif-symbol>
         (%onif-symbol base-symbol id)
         onif-symbol?
         (base-symbol %onif-symbol-ref-base-symbol)
         (id %onif-symbol-ref-id))

     (define onif-symbol/onif-symbol? onif-symbol?)

     (define onif-symbol/ref-symbol %onif-symbol-ref-base-symbol)

     (define onif-symbol
       (case-lambda
         (() (onif-symbol '_))
         ((base-symbol)
          (let ((id *id-counter*))
            (set! *id-counter* (+ *id-counter* 1))
            (%onif-symbol base-symbol *id-counter*)))))

      (define (onif-symbol->symbol osym);naive
        (string-append
          (symbol->string (%onif-symbol-ref-base-symbol osym))
          "-!"
          (number->string (%onif-symbol-ref-id osym))))))
