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
     onif-like-asm/convert-functions
     onif-like-asm/make-global-ids-box)

   (begin
     (define (onif-like-asm/make-global-ids-box)
       (list '() 0))

     (define (%atom-conv register code)
         (cond
           ((char? code)
            `(SET! (R ,register) code))
           (else
              (error "TBA 3"))))

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
                   `(SET! ,offset ,(+ org-offset x))));<= To change Register ???
            (else
              `(SET! (R ,offset) (GLOBAL ,var))))))

     (define (%expand-arg exps offset last-meta-info global-ids-box onif-symbol-hash)
         (let loop ((code exps)
                    (offset offset)
                    (rev-res '()))
           (display "CODE:")(display code)(newline)
           (cond
             ((null? code) (reverse rev-res))
             ((or (onif-symbol? (car code)) (symbol? (car code)));symbol
              (let* ((var-info;(SET! target from)
                      (%var-ref
                        (car code)
                        (%meta-info-ref last-meta-info 'local-ids)
                        offset
                        (%meta-info-ref last-meta-info 'local-register-offset)))
                     (var-position (caddr var-info))
                     (var-code
                       (cond
                         ((not (and (pair? var-position)
                                    (eq? (car var-position) 'GLOBAL)))
                          var-info)
                         ((assq (cadr var-position) (car global-ids-box))
                          => (lambda (id)
                               `(SET! ,(cadr var-info) (G ,(cadr var-position)))));TODO:FIX
                         ((cadr global-ids-box)
                          =>
                          (lambda (id)
                              (set-car!
                                global-ids-box
                                (cons
                                  (list (cadr var-position) (+ id 1))
                                  (car global-ids-box)))
                              (set-cdr!
                                global-ids-box
                                (+ 1 id))
                              `(SET! ,(cadr var-info) (G ,id))))
                         (else
                           (error "Unexpected variable" var-position)))))
              (display "???")(display (list var-code))(newline)
              ;VAR-CODE IS <UNSPECIFIED>. FIX IT.
              (loop
                (cdr code)
                (+ offset 1)
                (cons
                  var-code
                  rev-res))))
             ((not (pair? (car code)));atom
              (loop
                (cdr code)
                (+ offset 1)
                (cons (%atom-conv offset code) rev-res)))
             ((%lfun? (caar code) onif-symbol-hash);LOAD FUN
              (let* ((make-env-vector `(MAKE-VECTOR (R ,offset)
                                                    ,(length (caddar code))))
                     (local-ids (%meta-info-ref last-meta-info 'local-ids))
                     (registers
                       (map
                         (lambda (x)
                           (+ offset 1 (cadr (assq  x local-ids))))
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
               global-ids-box
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
                          global-ids-box
                          onif-symbol-hash))
                       (operation-code
                         (%operation-conv operator register-offset)))
                  (append arg-code operation-code))))
         (else
           (cond
             (else
               (display (vector "?" (onif-idebug-icode->code code)))(newline)
               (error "TBA! 1"))))))

     (define (onif-like-asm/convert-functions
               meta-functions
               offset
               global-ids-box
               onif-symbol-hash)
      (let loop ((functions meta-functions)
                 (offset offset)
                 (res '()))
         (if (null? functions)
           res
           (let* ((meta-function (cadar functions))
                  (bindings (cadr meta-function))
                  (meta-info (caddr meta-function))
                  (local-ids
                    (%meta-info-ref meta-info 'local-ids))
                  (body (cadddr meta-function))
                  (function-address offset))
                 (display "BODY::")(display (onif-idebug-icode->code  body '(not-contain-meta-info #t)))(newline)

               (display "META-INFO=")(display meta-info)(newline)
               (let* ((binding-list '());TBA
                      (register-position
                         (map
                           (lambda (apair)
                             (list
                               (car apair)
                               (+ (cadr apair) offset)))
                           local-ids))
                      (_res (append
                              res
                        (onif-like-asm/convert body
                                               offset
                                               meta-info
                                               global-ids-box
                                               onif-symbol-hash))))
                 (loop (cdr functions) offset _res))))))))
