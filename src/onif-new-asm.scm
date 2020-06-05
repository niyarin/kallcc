(include "./onif-misc.scm")
(include "./onif-idebug.scm")
(include "./onif-symbol.scm")
(include "./lib/thread-syntax.scm")

(define-library (onif new-asm)
   (import (scheme base) (scheme cxr) (scheme list) (scheme sort)
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
              (loop (cdr code)
                    (cons '(COMMENT "BODY-START")
                         (cdr rev-res))))
             ((and (eq? (caar code) 'SET!)
                   (equal? (cadar code) (caddar code)))
              (loop (cdr code) rev-res))
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
           ((and (integer? code) (< code 128))
              (error "TBA 3" (onif-idebug-icode->code code)))
           (else (error "TBA 3" (onif-idebug-icode->code code)))))

     (define (%asm-var var to-register-offset lock-registers local-register global-ids-box)
       (cond
         ((assq var local-register)
          => (lambda (l)
               `((COMMENT ((R ,to-register-offset) <= ,(onif-idebug-icode->code var)))
                 (SET! (R ,to-register-offset) (R ,(cadr l))))))
         ((list (list 'SET!
                      `(R ,to-register-offset)
                      (%global-ref! var global-ids-box))))))

     (define (%safety-register base offset lock-offsets)
       (let loop ((offset offset))
         (if (memq (+ base offset) lock-offsets)
           (loop (+ offset 1))
           (+ base offset))))

     (define (%n-safety-registers base offset lock-offsets n)
       (let loop ((offset offset)
                  (rev-res '())
                  (n n))
         (cond
           ((zero? n) (reverse rev-res))
           ((not (memq (+ base offset) lock-offsets))
            (loop (+ offset 1)
                  (cons (+ base offset) rev-res)
                  (- n 1)))
           (else (loop (+ offset 1) rev-res n)))))

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
         ;;返り値は順方向
         (let* ((addr-reg (%safety-register to-register-offset 1 lock-registers))
                (env-reg (%safety-register to-register-offset 2 (cons addr-reg lock-registers)))
                (null-reg (%safety-register to-register-offset 3 (cons* env-reg addr-reg lock-registers)))
                (out-env-reg (%use-registers-ref-env use-registers))
                (env-reg
                  (if (null? out-env-reg)
                    null-reg
                    out-env-reg))
                (null-set
                  (if (null? out-env-reg)
                    `((SET! (R ,null-reg) ()))
                    '()))
                (make-closure-addr
                 `((SET! (R ,addr-reg) (M2 ,(%make-symbol "FUN" (cadr code))))))
                (make-closure
                  `((MAKE-CLOSURE (R ,env-reg)
                                  (R ,addr-reg)
                                  (R ,to-register-offset)))))
            (append  null-set make-closure-addr make-closure)))

     (define (%asm-arg1 code use-registers to-register-offset
                        lock-registers local-register global-ids-box
                        onif-symbol-hash)
         ;;返り値は順方向
         (cond
           ((onif-misc/const? code) `(,(%asm-conv to-register-offset code)))
           ((onif-misc/var? code) (%asm-var code to-register-offset
                                            lock-registers local-register global-ids-box))
           ((null? (car code))
            (%asm-arg1 (cadr code) use-registers to-register-offset
                       lock-registers local-register global-ids-box))
           ((%lfun? (car code) onif-symbol-hash)
            (%asm-lfun code use-registers to-register-offset
                       lock-registers local-register global-ids-box))
           ((onif-misc/quote-operator? (car code) onif-symbol-hash)
               (if (null? (cadr code))
                 `((SET! (R ,to-register-offset) ()))
                 (error "TBA!!")))
           (else (error "TBA33" (onif-idebug-icode->code code)))))

     (define (%asm-args args use-registers to-register-offset
                        lock-registers local-register global-ids-box onif-symbol-hash)
       ;;返り値は順方向
       (let loop ((args args)
                  (lock-registers lock-registers)
                  (r (%safety-register to-register-offset 0 lock-registers))
                  (res '()))
         (if (null? args)
           res
           (loop (cdr args)
                 (cons r lock-registers)
                 (%safety-register r 1 lock-registers)
                 (append res
                         (%asm-arg1 (car args) use-registers r lock-registers local-register global-ids-box onif-symbol-hash))))))

     (define (%asm-define code use-registers register-offset
                          lock-registers local-register global-ids-box onif-symbol-hash)
       (let ((global (%global-ref! (caddr code) global-ids-box))
             (body (%asm-arg1 (cadddr code) use-registers register-offset
                              lock-registers local-register global-ids-box onif-symbol-hash))
             (continuation (cadr code)))
         (if (and (null? continuation) (null? local-register))
           `(,@body
              (SET! ,global (R ,register-offset))
              (CONCATENATE-BODY))
           `(,@body
              (SET! ,global (R ,register-offset))
              (SET! (R 0) ())
              (CALL 1)))))

; (define (onif-new-asm/convert code use-registers register-offset lock-registers local-register env global-ids-box jump-box onif-symbol-hash)
     (define (%asm-if code register-offset use-registers
                      lock-registers local-register jump-box env global-ids-box onif-symbol-hash)
        (onif-idebug/debug-display code)(newline)
        (let ((test (%asm-arg1 (cadr code) use-registers register-offset
                              lock-registers local-register global-ids-box onif-symbol-hash))
              (true (onif-new-asm/convert
                      (caddr code) use-registers register-offset
                      lock-registers local-register env global-ids-box jump-box onif-symbol-hash))
              (false (onif-new-asm/convert
                       (cadddr code) use-registers register-offset
                       lock-registers local-register  env global-ids-box jump-box onif-symbol-hash))
              (jsymbol (string->symbol
                         (string-append
                           "if" (number->string (car jump-box))))))
          (append test
                  `((IFNOT (R ,register-offset) ,jsymbol))
                  false
                  `((ELSE ,jsymbol))
                  true
                  `((ENDIF ,jsymbol)))))

     (define (%asm-operator ope code use-registers register-offset lock-registers local-register global-ids-box onif-symbol-hash)
        (let* ((args (%asm-args (cdr code) use-registers register-offset lock-registers local-register global-ids-box onif-symbol-hash))
               (cont (%safety-register register-offset 0 lock-registers))
               (r1 (%safety-register register-offset 0 (cons cont lock-registers)))
               (r2 (%safety-register register-offset 0 (cons* cont r1 lock-registers)))
               (r3 (%safety-register register-offset
                                     0
                                     (cons* cont r1 r2 lock-registers))))
          (case ope
            ((CONS)
             `(,@args
               (COMMENT ARGS "^" ,ope ,(onif-idebug-icode->code (cddr code)))
                (,ope (R ,r1)
                      (R ,r2)
                      (R ,r3))
                (SET! (R ,r1) (R ,r3))))
            ((CAR CDR)
              `(,@args
               (COMMENT ARGS "^" ,ope ,(onif-idebug-icode->code (cddr code)))
               (,ope (R ,r1)
                     (R ,r1))))
            ((FX+ FX- FX<?)
             `(,@args
               (COMMENT ARGS "^" ,ope ,(onif-idebug-icode->code (cddr code)))
               (,ope (R ,r1)
                    (R ,r2)
                    (R ,r1))))
            ((EQ?)
              `(,@args
               (COMMENT ARGS "^" ,ope ,(onif-idebug-icode->code (cddr code)))
               (EQ? (R ,r1) (R ,r2) (R ,r1))))
            (else (error "AAAAAAAAAAAAAAAAAAAAAAA" ope)))))

      ;(%asm-arg1 code use-registers to-register-offset
      ;lock-registers local-register global-ids-box onif-symbol-hash)
     (define (%asm-fun-call code use-registers register-offset
                        lock-registers local-register env
                        global-ids-box jump-box onif-symbol-hash)
       (let* ((args (%asm-args (cdr code) use-registers
                               2 (cons 0 lock-registers) local-register
                               global-ids-box onif-symbol-hash))
              (lock-registers (%n-safety-registers 2 0
                                                   lock-registers
                                                   (length (cdr code))))
              (mvs (map (lambda (to from)
                                `(SET! (R ,to) (R ,from)))
                        (map (lambda (x) (+ x 2)) (iota (length (cdr code))))
                        lock-registers))
              (op (%asm-arg1 (car code) '() 0 lock-registers
                              local-register global-ids-box onif-symbol-hash)))
            (append '((COMMENT "FUN CALL")) args op mvs `((CALL ,(length (cdr code)))))))

     (define (onif-new-asm/convert code use-registers register-offset lock-registers local-register env
                                   global-ids-box jump-box onif-symbol-hash)
       (cond
         ((or (onif-misc/const? code)
              (onif-misc/var? code)) (error "TBA3"))
         ((onif-misc/define-operator? (car code) onif-symbol-hash)
          (%asm-define code use-registers register-offset lock-registers local-register global-ids-box onif-symbol-hash))
         ((onif-misc/if-operator? (car code) onif-symbol-hash)
          (%asm-if code register-offset use-registers lock-registers local-register jump-box env global-ids-box onif-symbol-hash))
         ((onif-misc/ref-operations (car code) onif-symbol-hash)
          => (lambda (ope)
               (append
                  (%asm-operator ope code use-registers register-offset
                                 lock-registers local-register global-ids-box onif-symbol-hash)
                  (list
                    ;;継続の結果を渡すだけ。長さ1。
                    '(COMMENT "MOVE RESUL")
                   `(SET! (R 2)
                          (R ,(cadr (%n-safety-registers register-offset
                                                         0 lock-registers 2))))
                    '(CALL 1)))))
         (else
           (%asm-fun-call code use-registers register-offset lock-registers local-register env
                                   global-ids-box jump-box onif-symbol-hash))))

   (define (%ref-use-vars body onif-symbol-hash)
     (delete-duplicates
        (cond
         ((onif-misc/const? body) '())
         ((onif-misc/var? body) (list body))
         ((onif-misc/if-operator? (car body) onif-symbol-hash)
          (->> (append (caddr body) (cadddr body))
               (cons (cadr body))
               (filter onif-misc/var?)))
         ((onif-misc/ref-operations (car body) onif-symbol-hash)
          (filter onif-misc/var? (cdr body)))
         (else (filter onif-misc/var? body)))))

   (define (%%ref-use-free-vars body frame stack onif-symbol-hash)
      (let* ((use-vars (%ref-use-vars body onif-symbol-hash))
             (use-vars (lset-difference eq? use-vars frame)))
         (filter-map
           (lambda (var)
                (->> (filter-map
                       (lambda (cell index)
                          (let ((lin (list-index (lambda (x) (eq? var x)) cell)))
                            (if lin
                              (list var index lin)
                              #f)))
                        stack (iota (length stack)))
                     ((lambda (x)
                       (if (null? x)
                         #f
                         (car x))))))
              use-vars)))

   (define (%ref-use-free-vars body frame stack onif-symbol-hash)
      (let ((free-vars (%%ref-use-free-vars body frame stack onif-symbol-hash)))
        (list-sort (lambda (x y)
                     (or (< (cadr x) (cadr y))
                         (and (= (cadr x) (cadr y))
                              (< (caddr x) (caddr x)))))
                    free-vars)))

   (define (%gen-free-vars-expand free-vars tmp-register register-map stack-for-debug)
      (let loop ((ls free-vars)
                 (current-stack-cell 0)
                 (stk stack-for-debug)
                 (rev-res '()))
        (cond
          ((null? ls) (cons* '(SET! (R 0) (R 1))
                             `(CAR (R 0) (R ,tmp-register) )
                             (if (not (null? stk)) `(COMMENT ,(list 'STACK-NOT-EMP
                                                                    (onif-idebug-icode->code stk))) `(COMMENT ""))
                             `(COMMENT STACK-CELL ,(onif-idebug-icode->code (car stack-for-debug)))
                             (reverse rev-res)))
          ((not (eq? (cadar ls) current-stack-cell))
           (loop ls
                 (+ current-stack-cell 1)
                 (cdr stk)
                 (cons* `(COMMENT ,(list "STACK CELL"
                                         (onif-idebug-icode->code (cadr stk))))
                         `(CAR (R 0) (R ,tmp-register))
                         `(CDR (R 0) (R 0))
                         rev-res)))
          (else
            (loop (cdr ls)
                  current-stack-cell
                  stk
                  (cons `(VECTOR-REF (R ,tmp-register)
                                     ,(caddar ls)
                                     (R ,(cadr (assq (caar ls) register-map))))
                        rev-res))))))

   (define (%asm-fun id-fun use-registers register-offset lock-registers local-register
                     env global-ids-box jump-box onif-symbol-hash)
      (let* ((f-id (car id-fun))
             (meta-function (cadr id-fun))
             (meta-info (caddr meta-function))
             (formals (cadr meta-function))
             (formals-length (length formals))
             (body (cadddr meta-function))
             (formals-regs (->> (iota formals-length)
                                (map (lambda (x) (+ x 2)))))
             (frame-copy (->> formals-regs
                              (map (lambda (x)
                                     `(VECTOR-SET! (R 0) ,(- x 2) (R ,x))))))
             (local-stack (cadr (assq 'stack meta-info)));;NOT CONTAIN THIS CELL
             (free-vars (%ref-use-free-vars body formals local-stack onif-symbol-hash))
             (free-vars-local-register
                 (map (lambda (r v) (list (car v) (+ r formals-length 2)))
                      (iota (length free-vars))
                      free-vars))
             (free-vars-lock-register (map cadr free-vars-local-register))
             (free-vars-expand
               (if (null? free-vars)
                  '()
               (%gen-free-vars-expand free-vars
                                      (%safety-register (+ 2 (length formals) (length free-vars-local-register)) 0 lock-registers)
                                      free-vars-local-register
                                      local-stack ;DEBUG
                                      )))
             (new-local-register
               (append
                  free-vars-local-register
                  (map (lambda (x i)
                         (list x (+ i 2)))
                       formals
                    (iota (length formals)))))
             (new-lock-registers
               (append free-vars-lock-register
                         (cons 1 (append formals-regs lock-registers))))
             (another-register
               (%safety-register (+ (length free-vars) (length formals) 2) 0 new-lock-registers))
             (asm-body
               (onif-new-asm/convert
                 body use-registers register-offset
                 new-lock-registers
                 new-local-register
                 (cons (caddr meta-function) env)
                 global-ids-box jump-box onif-symbol-hash)))
           ; (display "FVAR >>")(display free-vars-expand)(newline)
           ; (display "FORMALS >>")(display formals)(newline)
           ; (display "FREE VARS")(display free-vars)(newline)
            ;(display "RRR")(display another-register)(newline)(newline)
         (append (list `(DEFUN ,(%make-symbol "FUN" f-id))
                       '(CLOSURE-ENV (R 0) (R 1))
                       `(COMMENT ,(list 'formals (onif-idebug-icode->code formals))))
                 free-vars-expand
                 (list `(VECTOR ,(length formals) (R 0)))
                 frame-copy
                 (list `(CONS (R 0) (R 1) (R ,another-register))
                       `(SET!  (R 1) (R ,another-register)))
                 (cons '(COMMENT "FUNCTION BODY") asm-body))))

   (define (onif-new-asm/asm-fun functions use-registers register-offset lock-registers local-register
                                 env global-ids-box jump-box onif-symbol-hash)
     (let loop ((functions functions)
                (res '()))
       (unless (null? functions) (display "!")(newline)(onif-idebug/debug-display (car functions)  '(not-contain-meta-info #t))(newline))
       (if (null? functions)
         res
         (loop (cdr functions)
               (append res
                       (%asm-fun (car functions) use-registers register-offset lock-registers local-register
                                 env global-ids-box jump-box onif-symbol-hash))))))))
