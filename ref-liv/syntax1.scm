(define-library (kallcc-core syntax1)
  (import (onif-lib core))
  (export let)
  (begin
    (define-syntax let;simple
      (syntax-rules ()
        ((let ((variable init) ...) bodies ...)
         ((lambda (variable ...) bodies ...) init ...))))
    ))
