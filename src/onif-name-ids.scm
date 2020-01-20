(include "./onif-meta-lambda.scm")
(include "./onif-misc.scm")

(define-library (onif name-ids)
   (import (scheme base)
           (scheme cxr)
           (onif misc)
           (onif meta-lambda))
   (export name-ids/make-name-local-ids)
   (begin
     (define (%make-name-local-ids code not-first-lambda offset onif-symbol-hash)
       (cond
         ((not (pair? code)) code)
         ((and not-first-lambda
               (onif-misc/lambda-meta-operator? (car code) onif-symbol-hash))
          (onif-meta-lambda/update-meta-info-body
            code
            'local-ids not-first-lambda
            (%make-name-local-ids
              (cadddr code)
              not-first-lambda
              offset
              onif-symbol-hash)))
         ((onif-misc/lambda-meta-operator? (car code) onif-symbol-hash)
          "META LAMBA MUST ALLOWS ONLY 1 ARGUMENT."
          (let* ((contain-symbols
                  (cadr (assq 'contain-symbols (caddr code))))
                 (local-ids
                   (do ((id offset (+ id 1))
                        (_contain-symbols
                          contain-symbols
                          (cdr _contain-symbols))
                        (res '() (cons (list (car _contain-symbols) id) res)))
                       ((null? _contain-symbols) res)))
                 (new-body
                   (%make-name-local-ids
                     (cadddr code)
                     local-ids
                     offset
                     onif-symbol-hash)))
              (onif-meta-lambda/update-meta-info-body
                code
                'local-ids local-ids
                new-body)))
         (else
           (map
             (lambda (x)
               (%make-name-local-ids
                 x
                 not-first-lambda
                 offset
                 onif-symbol-hash))
             code))))

     (define (name-ids/make-name-local-ids code onif-symbol-hash)
       (%make-name-local-ids code #f 0 onif-symbol-hash))))
