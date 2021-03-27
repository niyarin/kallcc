(define-library (niyarin preder)
  (import (scheme base) (scheme write))
  (export (rename and* and)
          (rename not* not)
          (rename or* or)
          (rename eq?* eq?)
          (rename equal?*  equal?)
          comp)
  (begin
    (define (and* . args)
      (lambda (x)
        (let loop ((args args))
          (cond
            ((null? args) #t)
            (((car args) x) (loop (cdr args)))
            (else #f)))))

    (define (or* . args)
      (lambda (x)
        (let loop ((args args))
          (cond
            ((null? args) #f)
            (((car args) x) #t)
            (else (loop (cdr args)))))))


    (define (comp* args)
      (if (null? args)
              values
              (lambda (x) ((comp* (cdr args)) ((car args) x)))))

    (define (comp . args) (comp* args))

    (define (eq?* obj) (lambda (x) (eq? obj x)))

    (define (equal?* obj) (lambda (x) (equal? obj x)))

    (define (not* pred)
      (lambda (x) (not (pred x))))))
