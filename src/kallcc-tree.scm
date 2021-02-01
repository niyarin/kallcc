(define-library (kallcc tree)
  (import (scheme base) (scheme list) (scheme vector) (scheme write))
  (export (rename any* any)
          (rename update* update))
  (begin
    ;;TODO:線形更新も追加したい
    (define (any* pred tree . accessors)
      ;;unsupport circular object
      ;;accessors... TBW
      (let loop ((tree tree))
        (cond
          ((list? tree)
           (or (any pred tree)
               (any loop tree)))
          ((vector? tree) (vector-any pred tree))
          ;;pair?
          (else #f))))

    (define (update* pred update-proc tree . accessors)
      (let loop ((tree tree))
        (cond
          ((list? tree)
           (map (lambda (x)
                 (let ((y (loop x)))
                   (if (pred y) (update-proc y) y)))
               tree))
          (else tree))))))
