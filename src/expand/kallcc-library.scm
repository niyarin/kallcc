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

(define-library (kallcc expand library)
  (import (scheme base) (scheme list))
  (export library? lassq body update apply-body updaten)
  (begin
    (define (library? object)
      (and (list? object)
           (list? (car object))
           (every symbol? (car object))
           (every (lambda (x) (and (pair? x) (symbol? (car x))))
                  (cadr object))))

    (define (lassq  key library . default)
        (cond
          ((not library) (error "Namespace is null"))
          ((assq key (cadr library)) => cadr)
          ((not (null? default)) (car default))
          (else (error "[nassq-error] Namespace doesn't have key."
                       key library))))

    (define (body library)
      (lassq 'body library))

    (define (update library key val)
      (list (car library)
            (cons (list key val) (cadr library))))

    (define (updaten library . args)
      (let loop ((library-values (cadr library))
                 (args args))
        (if (null? args)
          (list (car library) library-values)
          (loop (cons (list (car args) (cadr args)) library-values)
                (cddr args)))))

    (define (apply-body library fn)
      (let ((body* (body library)))
        (update library 'body (fn body*))))))
