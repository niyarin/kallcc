(define-library (niyarin preder)
  (import (scheme base))
  (export (rename and* and)
          (rename not* not)
          (rename or* or))
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

    (define (not* pred)
      (lambda (x) (not (pred x))))))
