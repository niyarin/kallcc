(include "./onif-expand.scm")

(define-library (onif phases)
   (import (scheme base)
           (onif misc)
           (onif expand))
   (export onif-phases/pre-expand)
   (begin
     (define (onif-phases/pre-expand code global)
       (let* ((expression-begin-removed
                (apply
                  append
                  (map (lambda (expression)
                         (onif-expand/remove-outer-begin expression global))
                       code)))
              (namespaces
                 (onif-expand/separate-namespaces
                   expression-begin-removed
                   global)))
             namespaces))))
