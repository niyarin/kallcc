(include "../src/onif-expand.scm")
(include "../src/onif-scm-env.scm")

(define-library (onif test expand)
   (import (scheme base)
           (scheme write)
           (srfi 78) ;Lightweight testing
           (srfi 125);R7RS large (scheme hash)
           (onif scm env)
           (onif expand))

   (export onif-test-expand)
   (begin
      (define (%test-expand);TODO:
        (let ((expand-environment 
                (onif-expand-environment))
              )
         (display
             (onif-expand
               '(lambda (a b) a)
               (onif-scm-env-tiny-core)
               '()
               expand-environment)
             )(newline)
          ))

      (define (%test-create-expand-environment)
         (let ((expand-environment (onif-expand-environment)))
           (check 
             (not 
               (null?
                 (assv 'onif-scm-env-tiny-core expand-environment)))
               => #t)))

      (define (onif-test-expand)
        (%test-create-expand-environment)
        (%test-expand)
        )

     ))
