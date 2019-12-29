(include "../src/onif-utils.scm")

(define-library (onif test utils)
   (import (scheme base)
           (scheme write)
           (srfi 78) ;Lightweight testing
           (onif utils))

   (export onif-test-utils)
   (begin
      (define (%test-onif-utils-formals->list)
           (check 
               (equal? (onif-utils-formals->list '(a b c)) '(a b c)) => #t)
           (check 
               (equal? (onif-utils-formals->list '()) '()) => #t)
           (check 
               (equal? (onif-utils-formals->list '(a b . c)) '(a b c)) => #t)
           (check 
               (equal? (onif-utils-formals->list 'c) '(c)) => #t))

      (define (onif-test-utils)
        (%test-onif-utils-formals->list))))
