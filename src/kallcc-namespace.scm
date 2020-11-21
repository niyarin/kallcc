(define-library (kallcc namespace)
  (import (scheme base) (scheme list))
  (export namespace? nassq)
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
          (else (error "Namespace doesn't have key."
                       key namespace))))))
