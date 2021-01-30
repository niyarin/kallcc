(define-library (kallcc tree)
  (import (scheme base) (scheme list) (scheme vector))
  (export any)
  (begin
    (define (any tree pred . accessors)
      ;;unsupport circular object
      ;;accessors... TBW
      (let loop ((tree tree) (res '()))
        (cond
          ((list? tree) (any pred tree))
          ((vector? tree) (vector-any pred tree))
          (else #f))))))

