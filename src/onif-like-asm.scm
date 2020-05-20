(include "./onif-misc.scm")
(include "./onif-idebug.scm")
(include "./onif-symbol.scm")

(define-library (onif like-asm)
   (import (scheme base) (scheme cxr) (scheme list)
           (onif idebug) (onif symbol) (onif misc)
           (scheme write);
           )
   (export onif-like-asm/convert
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
           ((integer? code)
            `(SET! (R ,register) ,code))
           ((null? code)
            `(SET! (R ,register) ,code))
           ((and (integer? code)
                 (< code 128))
              (error "TBA 3" (onif-idebug-icode->code code)))
           (else
              (error "TBA 3" (onif-idebug-icode->code code)))))

     (define %lfun?
       (onif-misc/make-check-onif-symbol-base-function 'LFUN))

     (define (%meta-info-ref meta-info key)
       (cond
         ((assq key meta-info) => cadr)
         (else '())))

     (define (%var-ref var local-ids offset org-offset)
       (cond
         ((assq var local-ids)
          => (lambda (x)
                (let ((org-offset (if (null? org-offset) 0 org-offset)))
                   `(SET! (R ,offset) (R ,(+ org-offset (cadr x)))))))
         (else `(SET! (R ,offset) (GLOBAL ,var)))))

     (define (%expand-arg exps offset last-meta-info global-ids-box onif-symbol-hash copy-local-offset)
         ;TODO:This function use registers more than necesarry.
         ;;    結局外でまるごとlocalコピーした(あとで直す)
         ;;コピー先の参照方法
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
                           copy-local-offset))
                        (var-position (caddr var-info))
                        (var-code
                          (cond
                            ((not (and (pair? var-position)
                                       (eq? (car var-position) 'GLOBAL)))
                             var-info)
                            ((assq (cadr var-position) (car global-ids-box))
                             => (lambda (id)
                                  `(SET! ,(cadr var-info) (G id))))
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
                 (loop (cdr code)
                       (+ offset 1)
                       (cons var-code rev-res))))
                ((not-pair? (car code));atom
                 (loop
                   (cdr code)
                   (+ offset 1)
                   (cons (%atom-conv offset (car code)) rev-res)))
                ((onif-misc/quote-operator? (caar code) onif-symbol-hash)
                 (loop
                   (cdr code)
                   (+ offset 1)
                   (cons `(SET! (R ,offset) ,(cadar code)) rev-res)))
                ((%lfun? (caar code) onif-symbol-hash);LOAD-FUN
                 (onif-idebug/debug-display (car code))(newline)
                 (let* ((make-env-vector `(VECTOR ,(length (caddar code))
                                                  (R ,offset)))
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
                        (make-closure-addr `((SET! (R ,(+ offset 1)) ,(cadar code))))
                        (make-closure
                          `(MAKE-CLOSURE
                             (R ,offset)
                             (R ,(+ offset 1))
                             (R ,offset))))
                     (loop
                       (cdr code)
                       (+ offset 1)
                       (append
                         (list make-closure)
                         set-env-vectors
                         (list make-env-vector)
                         make-closure-addr
                         (list `(COMMENT "Env vector (for closure)" ,(onif-idebug-icode->code (caddar code))))
                         rev-res))))
                (else (error "TBA 2" (onif-idebug-icode->code code))))))

     (define (%operation-conv operator register-offset)
       (case operator
         ((PAIR? CAR CDR)
          `((,operator (R ,(+ register-offset 1)) (R ,(+ register-offset 1)))))
         ((SET-CAR! SET-CDR!)
          `((COMMENT "SET-CAR/SET-CDR!")
            (,operator (R ,(+ register-offset 1)) (R ,(+ register-offset 1)))
            (SET! (R ,register-offset) (R ,(+ register-offset 1)));;TODO:check this code.(I think that this is unnecesarry.)
            (CALL 1)))
         ((CONS)
          `((,operator (R ,(+ register-offset 1))
                       (R ,(+ register-offset 2))
                       (R ,(+ register-offset 1)))
            (CALL 1)))
         (else
           (error "Undefined operator!" operator))))

     (define (%global-ref! name global-ids-box)
       (cond
         ((assq name (car global-ids-box))
           => (lambda (id) `(G ,id)))
         ((cadr global-ids-box)
           => (lambda (id)
                  (set-car!
                    global-ids-box
                    (cons
                      (list name (+ id 1))
                      (car global-ids-box)))
                  (set-cdr!  global-ids-box (list (+ 1 id)))
                  `(G ,id)))))

     (define (onif-like-asm/convert
               code
               register-offset
               last-meta-info
               global-ids-box
               onif-symbol-hash
               local-register-offset
               jump-box)
       (cond
         ((not-pair? code)
          (%atom-conv register-offset code))
         ((onif-misc/ref-operations (car code) onif-symbol-hash)
          => (lambda (operator)
                (let* ((arg-code
                        (%expand-arg
                          (cdr code)
                          register-offset
                          last-meta-info
                          global-ids-box
                          onif-symbol-hash
                          local-register-offset))
                       (operation-code
                         (%operation-conv operator register-offset)))
                  (append arg-code operation-code))))
         ((onif-misc/define-operator? (car code) onif-symbol-hash)
          (let ((global
                  (%global-ref! (cadr code) global-ids-box))
                (asm-body
                  (%expand-arg
                    (list (cadr code)
                          (cadddr code))
                    (+ register-offset 1)
                    last-meta-info
                    global-ids-box
                    onif-symbol-hash
                    local-register-offset)))
            `(,@asm-body
              (SET! ,global (R ,(+ register-offset 2)))
              (SET! (R 0) (R ,(+ register-offset 1)))
              ;;CALL length-of-arguments
              (CALL 1))))
         ((onif-misc/if-operator? (car code) onif-symbol-hash)
            ;;このフェーズではまだifをのこす
            ;;IF 内 IFが存在しないことを保証しているとする
            (let ((test (%expand-arg (list (cadr code))
                                     register-offset
                                     last-meta-info
                                     global-ids-box
                                     onif-symbol-hash
                                     local-register-offset))
                  (true (onif-like-asm/convert (caddr code)
                                               register-offset
                                               last-meta-info
                                               global-ids-box
                                               onif-symbol-hash
                                               local-register-offset
                                               jump-box))
                  (false (onif-like-asm/convert (cadddr code)
                                               register-offset
                                               last-meta-info
                                               global-ids-box
                                               onif-symbol-hash
                                               local-register-offset
                                               jump-box))
                  (jsymbol (string->symbol (string-append  "if" (number->string (car jump-box))))))
                (set-car! jump-box (+ (car jump-box) 1))
                (append test
                        `((IFNOT (R ,register-offset) ,jsymbol))
                        false
                        `((ELSE ,jsymbol))
                        true
                        `((ENDIF ,jsymbol))
                        )))

         (else
           (let ((arg-code (%expand-arg
                             (cdr code)
                             ;;Increment offset to store function pointer
                             (+ register-offset 1)
                             last-meta-info
                             global-ids-box
                             onif-symbol-hash
                             local-register-offset))
                 (set-function-pointer-code
                   (%expand-arg (list (car code))
                                register-offset
                                last-meta-info
                                global-ids-box
                                onif-symbol-hash
                                local-register-offset)))
             (append arg-code set-function-pointer-code `((CALL ,(length (cdr code)))))))))

     (define (onif-like-asm/convert-functions
               meta-functions
               offset
               global-ids-box
               onif-symbol-hash
               jump-box)
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
                  (copy-local-offset (length body))
                  (<remove-after>/copy-local
                     (map (lambda (key-val)
                                 `(SET! (R ,(+ (if (list? body)
                                                 copy-local-offset
                                                 0)
                                               (cadr key-val)))
                                        (R ,(+ (cadr key-val) 1))))
                               local-ids))
                  (_res (append res
                                (list `(DEFUN ,f-id)
                                      `(COMMENT "FUN" ,(onif-idebug-icode->code local-ids)
                                                      ,(onif-idebug-icode->code bindings)))
                                (cons '(COMMENT "Copy local")
                                      <remove-after>/copy-local)
                                (list '(COMMENT "FUNC BODY"))
                                (onif-like-asm/convert
                                         body
                                         offset
                                         meta-info
                                         global-ids-box
                                         onif-symbol-hash
                                         copy-local-offset
                                         jump-box))))
              (loop (cdr functions) offset _res)))))))
