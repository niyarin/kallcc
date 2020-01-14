(include "./onif-symbol.scm")

(define-library (onif idebug)
   (import (scheme base) (onif symbol))
   (export onif-idebug-icode->code)
   (begin
     (define (onif-idebug-icode->code icode)
       (cond 
         ((onif-symbol? icode)
            (onif-symbol->symbol icode))
         ((list? icode)
          (map (lambda (x) (onif-idebug-icode->code x)) icode))
         ((vector? icode)
          (vector-map (lambda (x) (onif-idebug-icode->code x)) icode))
         (else
           icode)))
     ))
