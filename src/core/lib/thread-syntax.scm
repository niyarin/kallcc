(define-library (niyarin thread-syntax)
   (import (only (scheme base) define-syntax syntax-rules begin ...))
   (export ->> ->)
   (begin
      (define-syntax ->>
         (syntax-rules ()
           ((_ x) x)
           ((_ x (op args ...) rest ...)
            (->> (op args ... x) rest ...))
           ((_ x op rest ...)
            (->> (op x) rest ...))))

      (define-syntax ->
         (syntax-rules ()
            ((_ x) x)
            ((_ x (op args ...) rest ...)
             (-> (op x args ...) rest ...))
            ((_ x op rest ...)
             (-> (op x) rest ...))))))
