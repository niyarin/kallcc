(define-library (onif utils)
   (import (scheme base))
   (export onif-utils-formals->list)
   (begin
     (define (onif-utils-formals->list formals)
         (cond 
           ((list? formals) formals)
           ((symbol? formals) (list formals))
           (else
             (let loop ((fs formals)(res '()))
               (cond
                 ((symbol? (cdr fs))
                  (reverse
                    (cons 
                       (cdr fs)
                       (cons
                          (car fs)
                          res))))
                 (else 
                   (loop (cdr fs) (cons (car fs) res))))))))
     ))
