(define-library (onif utils)
   (import (scheme base))
   (export onif-utils-formals->list onif-utils-for-each-cell1)
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

     (define (onif-utils-for-each-cell1 fn ls)
       (let loop ((cell ls))
         (if (not (pair? cell))
           '()
           (begin 
             (fn cell)
             (loop (cdr cell))))))
     ))
