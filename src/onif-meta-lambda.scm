(include "./onif-symbol.scm")
(include "./onif-idebug.scm")
(include "./onif-misc.scm")

(define-library (onif meta-lambda)
   (import (scheme base)
           (scheme list);SRFI 1
           (scheme cxr)
           (onif idebug) (scheme write)
           (onif misc)
           (onif symbol))
   (export onif-meta-lambda-conv
           onif-meta-lambda/update-meta-info
           onif-meta-lambda/update-body
           onif-meta-lambda/update-meta-info-body)

   (begin
    (define %lambda-operator?
       (onif-misc/make-check-onif-symbol-base-function 'lambda))

    (define %if-operator?
       (onif-misc/make-check-onif-symbol-base-function 'if))

     (define %lambda-meta-operator?
       (onif-misc/make-check-onif-symbol-base-function 'lambda-META))

     (define (%make-meta-info . options)
         (let ((stack
                 (cond ((assq 'stack options) => cadr)(else '())))
               (base
                 (cond ((assq 'base options) => cadr)(else #f)))
               (formals
                 (cond ((assq 'formals options) => cadr )(else '())))
               (contain-symbols
                 (cond ((assq 'contain-symbols options) => cadr )(else '()))))
            (list
              (list 'bind-names
                    (append (apply append stack)
                            formals))
              (list 'contain-symbols contain-symbols))))

     (define (onif-meta-lambda/update-meta-info meta-lambda-code key val)
        (list
         (car meta-lambda-code)
         (cadr meta-lambda-code)
         (cons
           (list key val)
           (caddr meta-lambda-code))
         (cadddr meta-lambda-code)))

     (define (onif-meta-lambda/update-body meta-lambda-code new-body)
       (list
         (car meta-lambda-code)
         (cadr meta-lambda-code)
         (caddr meta-lambda-code)
         new-body))

     (define (onif-meta-lambda/update-meta-info-body meta-lambda-code key val new-body)
       (list
         (car meta-lambda-code)
         (cadr meta-lambda-code)
         (cons
            (list key val)
            (caddr meta-lambda-code))
         new-body))

     (define (%conv-meta-lambda cps-code onif-symbol-hash stk onif-expand-env)
       (cond
         ((not (pair? cps-code)) cps-code)
         ((%lambda-operator? (car cps-code) onif-symbol-hash)
          (let* ((new-stk (cons (cadr cps-code) stk))
                 (new-body
                   (map
                      (lambda (body)
                        (%conv-meta-lambda
                          body
                          onif-symbol-hash
                          new-stk
                          onif-expand-env))
                      (cddr cps-code)))
                 (symbol-list
                   (map
                     (lambda (x)
                       (if (and (pair? x)
                                (%lambda-meta-operator? (car x) onif-symbol-hash))
                         (cadr (assq 'contain-symbols (caddr x)))
                         '()))
                     (apply append
                            (filter list? new-body))))
                 (contain-symbols
                   (apply append (cons (cadr cps-code) symbol-list))))
            `(,(onif-misc/onif-symbol-hash-ref onif-symbol-hash 'lambda-META)
             ,(cadr cps-code)
             ,(%make-meta-info
               `(stack ,stk)
               `(formals ,(cadr cps-code))
               `(contain-symbols  ,contain-symbols))
             .
             ,new-body)))
         (else
           (map
              (lambda (x)
                (%conv-meta-lambda x onif-symbol-hash stk onif-expand-env))
              cps-code))))

     (define (onif-meta-lambda-conv  cps-code onif-symbol-hash onif-expand-env-with-name-id)
       (%conv-meta-lambda
         cps-code
         onif-symbol-hash
         '()
         onif-expand-env-with-name-id))))
