(include "./onif-meta-lambda.scm")
(include "./onif-misc.scm")

(define-library (onif name-ids)
   (import (scheme base)
           (scheme cxr)
           (onif misc)
           (onif meta-lambda))
   (export name-ids/make-name-local-ids)
   (begin
     (define (%make-name-local-ids code bind-vars onif-symbol-hash)
      (cond
         ((not (pair? code)) code)
         ((onif-misc/lambda-meta-operator? (car code) onif-symbol-hash)
          (let* ((bind-vars (append (cadr code) bind-vars))
                 (name-ids
                   (do ((vars bind-vars (cdr vars))
                        (res '() (cons (list (car vars) id) res))
                        (id 0 (+ id 1)))
                     ((null? vars) res)))
                 (new-body
                   (%make-name-local-ids
                     (cadddr code)
                     bind-vars
                     onif-symbol-hash)))
                (onif-meta-lambda/update-meta-info-body
                  code
                  'local-ids
                  name-ids
                  new-body)))
         (else
           (map
             (lambda (x)
                 (%make-name-local-ids x bind-vars onif-symbol-hash))
             code))))

     (define (name-ids/make-name-local-ids code onif-symbol-hash)
      (%make-name-local-ids code '()  onif-symbol-hash))))
