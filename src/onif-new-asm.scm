(include "./onif-misc.scm")
(include "./onif-idebug.scm")
(include "./onif-symbol.scm")
(include "./lib/thread-syntax.scm")

(define-library (onif new-asm)
   (import (scheme base) (scheme cxr) (scheme list)
           (onif idebug) (onif symbol) (onif misc)
           (only (niyarin thread-syntax) ->> ->)
           (scheme write);
           )
   (export onif-new-asm/make-global-ids-box onif-new-asm/convert onif-new-asm/tune
           onif-new-asm/asm-fun )
   (begin
     (define %lfun?
       (onif-misc/make-check-onif-symbol-base-function 'LFUN))

     (define (onif-new-asm/make-global-ids-box)
       (let ((id-max-value 0)
             (symbol-id-alist '()))
          (list symbol-id-alist id-max-value)))

      (define (onif-new-asm/tune code)
         (let loop ((code code)
                    (rev-res '()))
           (cond
             ((null? code) (reverse rev-res))
             ((and (not (null? rev-res))
                   (eq? (caar rev-res) 'CONCATENATE-BODY)
                   (eq? (caar code) 'BODY-START))
              (loop (cdr code) (cdr rev-res)))
             (else (loop (cdr code) (cons (car code) rev-res))))))

     (define (%global-ref! name global-ids-box)
       (cond
         ((assq name (car global-ids-box))
           => (lambda (id-apair) `(G ,(cadr id-apair))))
         ((cadr global-ids-box)
           => (lambda (id)
                  (set-car!
                    global-ids-box
                    (cons
                      (list name id)
                      (car global-ids-box)))
                  (set-cdr!  global-ids-box (list (+ 1 id)))
                  `(G ,id)))))

     (define (%asm-conv register code)
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

     (define (%asm-var var to-register-offset lock-registers local-register global-ids-box)
       (cond
         ((assq var local-register)
          => (lambda (l)
               `((SET! (R ,to-register-offset) (R ,(cdr l))))))
         ((list 'SET!
                `(R ,to-register-offset)
                (%global-ref! var global-ids-box)))))

     (define (%safety-register base offset lock-offsets)
       (let loop ((offset offset))
         (if (memq (+ base offset) lock-offsets)
           (loop (+ offset 1))
           (+ base offset))))

     (define (%n-safety-registers base offset lock-offsets n)
       (let loop ((offset offset)
                  (res '())
                  (n n))
         (cond
           ((zero? n) (reverse res))
           ((not (memq (+ base offset) lock-offsets))
            (loop (+ offset 1)
                  (cons (+ base offset) res)
                  (- n 1)))
           (else (loop (+ offset 1) res n)))))

     (define (%make-symbol . args)
       (let loop ((args args)
                  (res-str ""))
         (cond
           ((null? args) (string->symbol res-str))
           ((string? (car args))
            (loop (cdr args)
                  (string-append res-str (car args))))
           ((number? (car args))
            (loop (cdr args)
                  (string-append res-str (number->string (car args)))))
           (else (error "TBA")))))

     (define %use-registers-ref-env car)

     (define (%asm-lfun code use-registers to-register-offset
                       lock-registers local-register global-ids-box)
         ;;local setの工夫がいりそう(レジスタとenvに変更がいる)
         (let* ((addr-reg (%safety-register to-register-offset 1 lock-registers))
                (env-reg (%safety-register to-register-offset 2 (cons addr-reg lock-registers)))
                (out-env-reg (%use-registers-ref-env use-registers))
                (make-env
                  (if (null? out-env-reg)
                    `((CONS '() '() (R ,env-reg)))
                    `((CONS '() (R ,out-env-reg) (R ,env-reg)))))
                (make-closure-addr
                 `((SET! (R ,addr-reg) (M2 ,(%make-symbol "FUN" (cadr code))))))
                (make-closure
                  `((MAKE-CLOSURE (R ,env-reg)
                                  (R ,addr-reg)
                                  (R ,to-register-offset)))))
              (append  make-env make-closure-addr make-closure)))

     (define (%asm-arg1 code use-registers to-register-offset
                        lock-registers local-register global-ids-box onif-symbol-hash)
         (cond
           ((onif-misc/const? code) `(,(%asm-conv to-register-offset code)))
           ((onif-misc/var? code) (list (%asm-var code to-register-offset
                                                  lock-registers local-register global-ids-box)))
           ((null? (car code))
            (%asm-arg1 (cadr code) use-registers to-register-offset
                       lock-registers local-register global-ids-box))
           ((%lfun? (car code) onif-symbol-hash)
            (%asm-lfun code use-registers to-register-offset
                       lock-registers local-register global-ids-box))
           (else (error "TBA33" code))))

     (define (%asm-args args use-registers to-register-offset
                        lock-registers local-register global-ids-box onif-symbol-hash)
       (let loop ((args args)
                  (lock-registers lock-registers)
                  (r (%safety-register to-register-offset 0 lock-registers))
                  (rev-res '()))
         (if (null? args)
           (reverse rev-res)
           (loop (cdr args)
                 (cons r lock-registers)
                 (%safety-register r 1 lock-registers)
                 (append (%asm-arg1 (car args) use-registers r lock-registers local-register global-ids-box onif-symbol-hash)
                         rev-res)))))

     (define (%asm-define code use-registers register-offset
                          lock-registers local-register global-ids-box onif-symbol-hash)
       (let ((global (%global-ref! (caddr code) global-ids-box))
             (body (%asm-arg1 (cadddr code) use-registers register-offset
                              lock-registers local-register global-ids-box onif-symbol-hash))
             (continuation (cadr code)))
         (if (null? continuation)
           `(,@body
              (SET! ,global (R ,register-offset))
              (CONCATENATE-BODY))
           (error "TBA define"))))

     (define (%asm-if code register-offset
                      lock-registers local-register jump-box global-ids-box)
        (let ((test (%asm-arg1 (cadr code) register-offset
                              lock-registers local-register global-ids-box))
              (true (%asm-arg1 (caddr code)
                               register-offset
                               lock-registers
                               local-register
                               global-ids-box))
               (false (%asm-arg1 (caddr code)
                                 register-offset
                                 lock-registers
                                 local-register
                                 global-ids-box))
              (jsymbol (string->symbol (string-append  "if" (number->string (car jump-box))))))
          (append test
                  `((IFNOT (R ,register-offset)))
                  false
                  `((ELSE ,jsymbol))
                  true
                  `((ENDIF ,jsymbol)))))


     (define (%asm-operator ope code use-registers register-offset lock-registers local-register global-ids-box onif-symbol-hash)
        (let* ((args (%asm-args (cdr code) use-registers register-offset lock-registers local-register global-ids-box onif-symbol-hash))
               (cont (%safety-register register-offset 0 lock-registers))
               (r1 (%safety-register register-offset 0 (cons cont lock-registers)))
               (r2 (%safety-register register-offset 0 (cons* cont r1 lock-registers))))
          (case ope
            ((FX+ FX- FX<?)
             `(,@args
               (,ope (R ,r1)
                    (R ,r2)
                    (R ,r1))))
            (else (error "AAAAAAAAAAAAAAAAAAAAAAA" ope)))))

     (define (onif-new-asm/convert code use-registers register-offset lock-registers local-register env
                                   global-ids-box jump-box onif-symbol-hash)
       (cond
         ((or (onif-misc/const? code)
              (onif-misc/var? code)) (error "TBA3"))
         ((onif-misc/define-operator? (car code) onif-symbol-hash)
          (%asm-define code use-registers register-offset lock-registers local-register global-ids-box onif-symbol-hash))
         ((onif-misc/if-operator? (car code) onif-symbol-hash)
          (%asm-if code register-offset lock-registers local-register jump-box global-ids-box))
         ((onif-misc/ref-operations (car code) onif-symbol-hash)
          => (lambda (ope)
               (append
                  (%asm-operator ope code use-registers register-offset
                                 lock-registers local-register global-ids-box onif-symbol-hash)
                  (list
                    ;;継続の結果を渡すだけ。長さ1。
                    '(COMMENT "MOVE RESUL")
                   `(SET! (R ,(cadr (%n-safety-registers register-offset
                                                         0 lock-registers 2)))
                          (R 2))
                    '(CALL 1)))))
         (else (error "TBA conv" (car code)))))

   (define (%asm-fun id-fun use-registers register-offset lock-registers local-register
                     env
                     global-ids-box jump-box onif-symbol-hash)
      (let* ((f-id (car id-fun))
             (meta-function (cadr id-fun))
             (bindings (cadr meta-function))
             (binding-regs
               (->> (iota (length bindings))
                    (map (lambda (x)
                           (+ x 2)))))
             (new-local-register
               (map (lambda (x i)
                      (cons x (+ i 2)))
                    bindings
                    (iota (length bindings))))
             (body (cadddr meta-function))
             (asm-body
               (onif-new-asm/convert
                 body use-registers register-offset
                 (append (list 1) binding-regs lock-registers)
                 new-local-register
                 (cons (caddr meta-function) env)
                 global-ids-box jump-box onif-symbol-hash)))
            (cons* `(DEFUN ,(%make-symbol "FUN" f-id))
                   '(CLOSURE-ENV (R 0) (R 1))
                   asm-body)))


   (define (onif-new-asm/asm-fun functions use-registers register-offset lock-registers local-register
                                 env global-ids-box jump-box onif-symbol-hash)
     (let loop ((functions functions)
                (res '()))
       (if (null? functions)
         res
         (loop (cdr functions)
               (append res
                       (%asm-fun (car functions) use-registers register-offset lock-registers local-register
                                 env global-ids-box jump-box onif-symbol-hash))))))))

