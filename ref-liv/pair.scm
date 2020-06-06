(define-library (onif-lib pair)
   (import (onif-lib core))
   (export caar cadr cdar cddr null?)
   (begin
     (define caar
       (lambda (x)
         (car (car x))))

     (define cadr
       (lambda (x)
          (car (cdr x))))

     (define cdar
       (lambda (x)
         (cdr (car x))))

     (define cddr
       (lambda (x)
         (cdr (cdr x))))

     (define null?
         (lambda (x)
           (eq? x '())))))
