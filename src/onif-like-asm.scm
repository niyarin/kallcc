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
       (let ((id-max-value 0)
             (symbol-id-alist '()))
          (list symbol-id-alist id-max-value)))

     (define (%atom-conv register code)
         (cond
           ((char? code)
            `(SET! (R ,register) ,code))
           ((null? code)
            `(SET! (R ,register) ,code))
           ((and (integer? code)
                 (< code 128))
              (error "TBA 3" code))
           (else
              (error "TBA 3" code))))

     (define %lfun?
       (onif-misc/make-check-onif-symbol-base-function 'LFUN))

     (define (%meta-info-ref meta-info key)
       (cond
         ((assq key meta-info)
          => cadr)
         (else
           '())))

     (define (%var-ref var local-ids offset org-offset)
       (let ((org-offset (if (null? org-offset) 0 org-offset)))
          (cond
            ((assq var local-ids)
             => (lambda (x)
                   `(SET! (R ,offset) (R ,(+ org-offset (cadr x))))))
            (else
              `(SET! (R ,offset) (GLOBAL ,var))))))

     (define (%expand-arg exps offset last-meta-info global-ids-box onif-symbol-hash)
         ;TODO:This function use registers more than necesarry.
         ;;    結局外でまるごとlocalコピーした(あとで直す)
            (let loop ((code exps)
                       (offset offset)
                       (rev-res '()))
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
                                   (list (+ 1 id)))
                                 `(SET! ,(cadr var-info) (G ,id))))
                            (else
                              (error "Unexpected variable" var-position)))))
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
                   (cons (%atom-conv offset (car code)) rev-res)))
                ((%lfun? (caar code) onif-symbol-hash);LOAD-FUN
                 (let* ((make-env-vector `(MAKE-VECTOR (R ,offset)
                                                       ,(length (caddar code))))
                        (local-ids (%meta-info-ref last-meta-info 'local-ids))
                        (registers
                          (map
                            (lambda (x)
                              (+ offset 1 (cadr (assq  x local-ids))))
                            (caddar code)))
                        (set-env-vectors
                          (onif-misc/map-indexed
                            (lambda (index x)
                              `(VECTOR-SET! (R ,offset) ,index (R ,x)))
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
                         (list `(COMMENT "Env vector (for closure)" ,(caddar code)))
                         rev-res))))
                (else
                  (error "TBA 2"
                         (onif-idebug-icode->code code))))))

     (define (%operation-conv operator register-offset)
       (case operator
         ((PAIR? CAR CDR)
          `((,operator (R ,register-offset) (R ,(+ register-offset 1)))))
         ((CONS)
          `((,operator (R ,register-offset) (R ,(+ register-offset 1)) (R ,(+ register-offset 2)))))
         (else
           (error "Undefined operator!" operator))))

     (define (onif-like-asm/convert
               code
               register-offset
               last-meta-info
               global-ids-box
               onif-symbol-hash)
       (onif-idebug/debug-display code)(newline)
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
           (let ((arg-code (%expand-arg
                             (cdr code)
                             ;;Increment offset to store function pointer
                             (+ register-offset 1)
                             last-meta-info
                             global-ids-box
                             onif-symbol-hash))
                 (set-function-pointer-code
                   (%expand-arg (list (car code))
                                register-offset
                                last-meta-info
                                global-ids-box
                                onif-symbol-hash)))
             (append arg-code set-function-pointer-code '((FRUN)))))))

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
           (let* ((f-id (caar functions));TODO:use!
                  (meta-function (cadar functions))
                  (bindings (cadr meta-function))
                  (meta-info (caddr meta-function))
                  (local-ids (%meta-info-ref meta-info 'local-ids))
                  ;local-ids:((symbol id) ...)
                  (register-position
                         (map (lambda (binding)
                                (list (car binding)
                                      (+ (cadr binding) offset)))
                              local-ids))
                  (body (cadddr meta-function))
                  (<remove-after>/copy-local
                     (map (lambda (key-val)
                                 `(SET! (R ,(+ (length local-ids) (cadr key-val)))
                                        (R ,(cadr key-val))))
                               local-ids))
                  (_res (append res
                                (list `(DEFUN ,f-id)
                                      `(COMMENT "FUN" ,local-ids))
                                <remove-after>/copy-local
                                (onif-like-asm/convert
                                         body
                                         offset
                                         meta-info
                                         global-ids-box
                                         onif-symbol-hash))))
              (loop (cdr functions) offset _res)))))))
