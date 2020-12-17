(include "./onif-meta-lambda.scm")
(include "./onif-misc.scm")

(define-library (onif name-ids)
   (import (scheme base)
           (prefix (kallcc misc) kmisc/)
           (onif misc)
           (onif meta-lambda))
   (export name-ids/make-name-local-ids)
   (begin
     (define (%make-name-local-ids bind-vars)
       (let loop ((i 0)
                  (vars bind-vars)
                  (res '()))
         (if (null? vars)
           res
           (loop (+ i 1) (cdr vars) (cons (list (car vars) i) res)))))

     (define (%add-meta-info-name-local-ids code bind-vars onif-symbol-hash)
      (cond
         ((not (pair? code)) code)
         ((onif-misc/lambda-meta-operator? (car code) onif-symbol-hash)
          (let* ((bind-vars (append (kmisc/formals->list (cadr code)) bind-vars))
                 (name-ids
                   (%make-name-local-ids bind-vars))
                 (new-body
                   (%add-meta-info-name-local-ids
                     (list-ref code 3) bind-vars onif-symbol-hash)))
                (onif-meta-lambda/update-meta-info-body
                  code 'local-ids name-ids new-body)))
         (else
           (map (lambda (x) (%add-meta-info-name-local-ids x bind-vars
                                                           onif-symbol-hash))
             code))))

     (define (name-ids/make-name-local-ids code onif-symbol-hash)
      (%add-meta-info-name-local-ids code '()  onif-symbol-hash))))
