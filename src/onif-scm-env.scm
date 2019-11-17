(include "./onif-symbol.scm")

(define-library (onif scm env)
   (cond-expand
     ((library (srfi 125))
         (import (scheme base)
                 (srfi 125)
                 (onif symbol)))
     ((library (scheme hash))
         (import (scheme base)
                 (scheme hash)
                 (onif symbol))))

   (export onif-scm-env-tiny-core)

   (begin

     (define %scm-env-tiny-core
       (let ((res #f))
         (lambda ()
           (if res
             res
            `((lambda . (built-in-lambda ,(onif-symbol 'LAMBDA)))
              (if . (built-in-if ,(onif-symbol 'IF)))
              (define . (built-in-define . ,(onif-symbol 'DEFINE)))
              (set! . (built-in-set! ,(onif-symbol 'SET!)))
              (quote . (built-in-set! ,(onif-symbol 'QUOTE)))
              (define-syntax . (built-in-set! ,(onif-symbol 'DEFINE-SYNTAX)))
              (let-syntax . (built-in-set! ,(onif-symbol 'LET-SYNTAX)))
              (begin . (built-in-begin ,(onif-symbol 'SET!))))))))

     (define (onif-scm-env-tiny-core)
         (alist->hash-table
           (%scm-env-tiny-core)
           eq?
         ))
     ))
