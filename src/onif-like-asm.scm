(include "./onif-misc.scm")
(include "./onif-idebug.scm")
(include "./onif-symbol.scm")

(define-library (onif like-asm)
   (import (scheme base)
           (scheme cxr)
           (scheme write);
           (onif idebug)
           (onif symbol)
           (onif misc))
   (export
     onif-like-asm/convert
     onif-like-asm/convert-function)
   (begin
     (define (%atom-conv register code)
         (cond
           ((char? code)
            `(SET! (R ,register) code))
           (else
              (error "TBA"))))

     (define %lfun?
       (onif-misc/make-check-onif-symbol-base-function 'LFUN))

     (define (%meta-info-ref meta-info key)
       (cond
         ((assq key meta-info)
          => cadr)
         (else
           '())))

     (define (%var-ref var local-ids offset org-offset)
       (let ((org-offset (if org-offset org-offset 0)))
          (cond
            ((assq var local-ids)
             => (lambda (x)
                   `(SET! ,offset ,(+ org-offset x))))
            (else
              `(SET! (R ,offset) (GLOBAL ,var))))))

     (define (%expand-arg exps offset last-meta-info onif-symbol-hash)
         (let loop ((code exps)
                    (offset offset)
                    (rev-res '()))
           (cond
             ((null? code) (reverse rev-res))
             ((or (onif-symbol? (car code)) (symbol? (car code)))
              (loop
                (cdr code)
                (+ offset 1)
                (cons
                  (%var-ref
                    (car code)
                    (%meta-info-ref last-meta-info 'local-ids)
                    offset
                    (%meta-info-ref last-meta-info 'local-register-offset))
                  rev-res)))
             ((not (pair? code))
              (loop
                (cdr code)
                (+ offset 1)
                (cons (%atom-conv offset code) rev-res)))
             ((%lfun? (caar code) onif-symbol-hash)
              (let* ((make-env-vector
                       `(MAKE-VECTOR ,(length (caddar code)) (R ,offset)))
                     (local-ids (%meta-info-ref last-meta-info 'local-ids))
                     (registers
                       (map
                         (lambda (x)
                           (+ offset 1 (cadr (assq  local-ids x))))
                         (caddar code)))
                     (set-env-vectors;順序は気にしなくて良い
                       (map
                         (lambda (x)
                           `(VECTOR-SET! (R ,offset) (R ,x)))
                         registers))
                     (make-closure
                       `(MAKE-CLOSURE
                          (R ,offset)
                          (F ,(cadar code))
                          (R ,offset))))
                  (loop
                    (cdr code)
                    (+ offset 1)
                    (append
                      (list make-closure)
                      set-env-vectors
                      (list make-env-vector)
                      rev-res))))
             (else
               (error "TBA 2"
                      (onif-idebug-icode->code code))))))

     (define (%operation-conv operator register-offset)
       (case operator
         ((PAIR? CAR CDR)
          `((,operator (R ,register-offset) (R ,(+ register-offset 1)))))))

     (define (onif-like-asm/convert
               code
               register-offset
               last-meta-info
               onif-symbol-hash)
       (cond
         ((not (pair? code))
          (%atom-conv register-offset code))
         ((onif-misc/ref-operations (car code) onif-symbol-hash)
          => (lambda (operator)
                (let* ((arg-code
                        (%expand-arg
                          (cdr code)
                          register-offset
                          last-meta-info
                          onif-symbol-hash))
                       (operation-code
                         (%operation-conv operator register-offset)))
                  (append arg-code operation-code))))
         (else
           (cond
             (else
               (display (vector "?" (onif-idebug-icode->code code)))(newline)
               (error "TBA"))))))

     (define (onif-like-asm/convert-function meta-functions offset onif-symbol-hash)
      (let loop ((functions meta-functions)
                  (offset offset)
                  (res '()))
         (if (null? functions)
           '()
           (let* ((function (cadar functions))
                  (bindings (cadr function))
                  (meta-info (caddr function))
                  (local-ids
                    (%meta-info-ref meta-info 'local-ids))
                  (body (cadddr function))
                  (function-address offset))
               (let* ()
                 (newline)
                 (display local-ids)(newline)
                 (newline)
                 (loop (cdr functions) offset res))))))))
