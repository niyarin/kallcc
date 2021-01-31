(define-library (onif opt names)
   (import (scheme base) (scheme list) (onif misc) (scheme cxr))
   (export onif-opt-names/calc-use-names onif-ope-names/remove-unnecessary-define)
   (begin
     (define (onif-opt-names/calc-use-names cps-meta-code onif-symbol-hash)
       (let* ((res '()))
          (let loop ((code cps-meta-code))
            (cond
              ((onif-misc/var? code) (set! res (cons code res)))
              ((not (pair? code)))
              ((onif-misc/define-operator? (car code) onif-symbol-hash)
               (loop (cadddr code)))
              ((onif-misc/lambda-meta-operator? (car code) onif-symbol-hash)
               (for-each loop (cdddr code)))
              (else (for-each loop code))))
          (delete-duplicates res)))

     (define (onif-ope-names/remove-unnecessary-define expression use-vars onif-symbol-hash)
         "Look only outermost expression."
         (cond
           ((not (pair? expression)) expression)
           ((onif-misc/define-operator? (car expression) onif-symbol-hash)
            (if (memq (caddr expression) use-vars)
              expression
              '()))
           (else expression)))))
