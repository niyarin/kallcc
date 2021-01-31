;;libraryに改名したい.

(define-library (kallcc namespace)
  (import (scheme base) (scheme list))
  (export namespace? nassq body update apply-body)
  (begin
    (define (namespace? object)
      (and (list? object)
           (list? (car object))
           (every symbol? (car object))
           (every (lambda (x) (and (pair? x) (symbol? (car x))))
                  (cadr object))))

    (define (nassq  key namespace . default)
        (cond
          ((not namespace) (error "Namespace is null"))
          ((assq key (cadr namespace)) => cadr)
          ((not (null? default)) (car default))
          (else (error "[nassq-error] Namespace doesn't have key."
                       key namespace))))

    (define (body namespace)
      (nassq 'body namespace))


    (define (update namespace key val)
      (list (car namespace)
            (cons (list key val) (cadr namespace))))

    (define (apply-body namespace fn)
      (let ((body* (body namespace)))
        (update namespace 'body (fn body*))))))
