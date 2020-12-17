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

     (define (%asm-const-conv register code)
       ;;TODO::ここのintegerの部分128の大小比較が変なので見直す
         (cond
           ((char? code)
            `(SET! (R ,register) ,code))
           ((integer? code)
            `(SET! (R ,register) ,code))
           ((null? code)
            `(SET! (R ,register) ,code))
           ((and (integer? code) (< code 128))
              (error "TBA 3" (onif-idebug-icode->code code)))
           (else (error "TBA (unsupported object type.)" (onif-idebug-icode->code code)))))

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
       (string->symbol
         (apply string-append
                (map (lambda (x)
                       (cond
                         ((string? x) x)
                         ((number? x) (number->string x))
                         (else (error "TBW"))))
                     args))))

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


     (define (%asm-byte-vector bv to-register-offset lock-registers)
       ;;外側という前提
       (let* ((r1 to-register-offset)
              (r2 (%safety-register to-register-offset 1 lock-registers))
              (r3 (%safety-register to-register-offset 2 lock-registers))
              (make-bv `(BYTEVECTOR
                         ,(bytevector-length bv)
                         (R ,r1))))

         (let loop ((i 0)
                    (res (list make-bv)))
           (if (< i (bytevector-length bv))
             (loop (+ i 1)
                   (cons* `(BYTEVECTOR-U8-SET! (R ,r1) (R ,r2) (R ,r3))
                          `(SET! (R ,r3) ,(bytevector-u8-ref bv i))
                          `(SET! (R ,r2) ,i)
                          res))
             (reverse res)))))

     (define (%asm-arg1 code use-registers to-register-offset
                        lock-registers local-register global-ids-box
                        onif-symbol-hash)
         ;;返り値は順方向
         (cond
           ((bytevector? code)
            (%asm-byte-vector code to-register-offset lock-registers))
           ((onif-misc/const? code) `(,(%asm-const-conv to-register-offset code)))
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
                 (error "TBW!")))
           (else (error "TBW33" (onif-idebug-icode->code code)))))

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
             (continuation (cadr code)))
         (if (and (null? continuation) (null? local-register))
           (let ((body (%asm-arg1 (cadddr code) use-registers register-offset
                                  lock-registers local-register global-ids-box onif-symbol-hash)))
           `(,@body
              (SET! ,global (R ,register-offset))
              (CONCATENATE-BODY)))
           ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;lockレジスターが完全でない
           (let* ((r0 (%safety-register register-offset 0 lock-registers))
                  (r1 (%safety-register register-offset 0 (cons r0 lock-registers)))
                  (cont-arg (%asm-arg1 (cadr code) use-registers r0
                                      lock-registers local-register global-ids-box onif-symbol-hash))
                  (body-arg (%asm-arg1 (cadddr code) use-registers r1
                                      lock-registers local-register global-ids-box onif-symbol-hash)))
             `(,@cont-arg
               ,@body-arg
                (SET! ,global (R ,r1))
                ;(SET! (R 0) ())
                (CALL 1))))))

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
          (set-car! jump-box (+ (car jump-box) 1))
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
            ((SET-CAR! SET-CDR!)
               `(,@args
                 (COMMENT ARGS "^" ,ope ,(onif-idebug-icode->code (cddr code)))
                 (,ope (R ,r1)
                       (R ,r2)
                       (R ,r3))))
            ((FX+ FX- FX* FX<? FX=? FXREMAINDER)
             `(,@args
               (COMMENT ARGS "^" ,ope ,(onif-idebug-icode->code (cddr code)))
               (,ope (R ,r1)
                     (R ,r2)
                     (R ,r1))))
            ((EQ?)
              `(,@args
               (COMMENT ARGS "^" ,ope ,(onif-idebug-icode->code (cddr code)))
               (EQ? (R ,r1) (R ,r2) (R ,r1))))
            ((MAKE-VECTOR)
             (if (integer? (caddr code))
               `(,@args
                 (COMMENT ARGS "^" ,ope ,(onif-idebug-icode->code (cddr code)))
                 (VECTOR ,(caddr code)) (R ,r1))
               `(,@args
                 (COMMENT ARGS "^" ,ope ,(onif-idebug-icode->code (cddr code)))
                 (VECTOR (R ,r1) (R ,r2))
                 (SET! (R ,r1) (R ,r2))
                 )))
               ;(error "TBW (make-vector variable) .(currently supported const-number.)")))
            ((VECTOR-SET!)
            `(,@args
               (COMMENT ARGS "^" ,ope ,(onif-idebug-icode->code (cddr code)))
               (VECTOR-SET! (R ,r1) (R ,r2) (R ,r3))))
            ((MAKE-BYTEVECTOR)
             `(,@args
               (COMMENT ARGS "^" ,ope ,(onif-idebug-icode->code (cddr code)))
               (BYTEVECTOR ,(caddr code)
                           (R ,r1)
                           (R ,r2))))
            ((BYTEVECTOR-U8-REF)
             `(,@args
                (COMMENT ARGS "^" ,ope ,(onif-idebug-icode->code (cddr code)))
                (BYTEVECTOR-U8-REF (R ,r1) (R ,r2) (R ,r3))
                (SET! (R ,r1) (R ,r3))))
            ((BYTEVECTOR-U8-SET!)
               `(,@args
                (COMMENT ARGS "^" ,ope ,(onif-idebug-icode->code (cddr code)))
                (BYTEVECTOR-U8-SET! (R ,r1) (R ,r2) (R ,r3))
                ;;;;;RET UNDEF?
                ))
            ((BYTEVECTOR-LENGTH)
             `(,@args
                (COMMENT ATGS "^" ,ope ,(onif-idebug-icode->code (cddr code)))
                (BYTEVECTOR-LENGTH (R ,r1) (R,r2))))
            (else (error "TBW?????????" ope)))))

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
              (op (%asm-arg1 (car code) use-registers 0 lock-registers
                              local-register global-ids-box onif-symbol-hash)))
          (append '((COMMENT "FUN CALL")) args op mvs `((CALL ,(length (cdr code)))))))

     (define (onif-new-asm/convert code use-registers register-offset lock-registers local-register env
                                   global-ids-box jump-box onif-symbol-hash)
       (cond
         ((or (onif-misc/const? code)
              (onif-misc/var? code))
          (onif-idebug/debug-display code)(newline)
          (error "TBA3" code))
         ((or (onif-misc/define-operator? (car code) onif-symbol-hash)
              (onif-misc/set!-operator? (car code) onif-symbol-hash))
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
                          (let ((l-index (list-index (lambda (x) (eq? var x))
                                                     cell)))
                            (if l-index (list var index l-index) #f)))
                        stack
                        (iota (length stack)))
                     ((lambda (x)
                       (if (null? x) #f (car x))))))
              use-vars)))

   (define (%ref-use-free-vars body frame stack onif-symbol-hash)
      (let ((free-vars (%%ref-use-free-vars body frame stack onif-symbol-hash)))
        (list-sort (lambda (x y)
                     (or (< (cadr x) (cadr y))
                         (and (= (cadr x) (cadr y))
                              (< (caddr x) (caddr x)))))
                    free-vars)))

   (define (%gen-free-vars-expand free-vars tmp-register register-map stack-for-debug)
     (display "!!!!!!!!!!!!!")(onif-idebug/debug-display  register-map)(newline)
     (display "????????????????")(onif-idebug/debug-display (map car free-vars))(newline)
      ;tmp-register
      (let loop ((ls free-vars);((<onif-symbol> <integer> <integer>) ...)
                 (current-stack-cell 0)
                 (stk stack-for-debug)
                 (rev-res '()))
        (cond
          ((null? ls) (cons* '(SET! (R 0) (R 1))
                             `(CAR (R 0) (R ,tmp-register))
                             (if (not (null? stk))
                               `(COMMENT ,(list 'STACK-NOT-EMP
                                               (onif-idebug-icode->code stk)))
                               `(COMMENT ""))
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

   (define (%asm-fun-header function-id formals body)
      `((DEFUN ,(%make-symbol "FUN" function-id))
        (COMMENT (lambda ,(onif-idebug-icode->code formals) ,(onif-idebug-icode->code body)))
        (CLOSURE-ENV (R 0) (R 1))
        (COMMENT (list formals ,(onif-idebug-icode->code formals)))))

   (define (%asm-fun-free-vars meta-function lock-registers symbol-hash)
      (let* ((formals (cadr meta-function))
             (formals-length (length formals))
             (body (list-ref meta-function 3))
             (local-stack (cadr (assq 'stack (list-ref meta-function 2))));;NOT CONTAIN THIS CELL
             (free-vars (%ref-use-free-vars body formals local-stack symbol-hash))
             (registers-for-free-vars (map (lambda (x) (+ x formals-length 2)) (iota (length free-vars))))
             (free-vars-local-register (map (lambda (free-var-box rnum) (list (car free-var-box) rnum))
                                            free-vars registers-for-free-vars))
             (free-vars-expand
               (if (null? free-vars)
                  '()
                  (%gen-free-vars-expand free-vars
                                        (%safety-register (+ 2 formals-length (length free-vars-local-register))
                                                          0 lock-registers)
                                        free-vars-local-register
                                        local-stack ;DEBUG
                                        )))
             (phase1-lock-registers (append registers-for-free-vars lock-registers)))
        (values free-vars-expand free-vars-local-register phase1-lock-registers (length free-vars))))

   (define (%asm-fun-make-env-frame meta-function local-registers lock-registers)
     (let* ((body (list-ref meta-function 3))
            (formals (cadr meta-function))
            (formals-length (length formals))
            (formals-regs (map (lambda (x) (+ x 2))
                               (iota formals-length)))
            (frame-copy (map (lambda (x) `(VECTOR-SET-INIT! (R 0) ,(- x 2) (R ,x)))
                              formals-regs))
            (new-local-register
               (append
                  local-registers
                  (map (lambda (x i) (list x (+ i 2)))
                       formals
                      (iota (length formals)))))
            (new-lock-registers
               (append '(1)
                       formals-regs
                       lock-registers)))
      (values (cons `(VECTOR ,formals-length (R 0)) frame-copy)
              new-local-register new-lock-registers (length formals))))

   (define (%asm-fun-lset id-fun use-registers register-offset lock-registers local-register
                          env global-ids-box jump-box onif-symbol-hash)
    (let* ((meta-function (cadr id-fun)))
      (append (%asm-fun-header (car id-fun) (cadr meta-function) (list-ref meta-function 3))
              )))

   (define (%asm-fun id-fun use-registers register-offset lock-registers local-register
                     env global-ids-box jump-box onif-symbol-hash)
      (let*-values (((free-vars-expand phase1-local-registers phase1-lock-registers phase1-register-shift)
                       (%asm-fun-free-vars (cadr id-fun) lock-registers onif-symbol-hash))
                    ((make-env-frame phase2-local-registers phase2-lock-registers phase2-register-shift)
                     (%asm-fun-make-env-frame (cadr id-fun) phase1-local-registers  phase1-lock-registers)))
        (let* ((meta-function (cadr id-fun))
               (another-register
                 (%safety-register (+ phase1-register-shift
                                      phase2-register-shift 2)
                                   0 phase2-lock-registers))
               (asm-body
                 (onif-new-asm/convert (list-ref meta-function 3) use-registers register-offset
                                       phase2-lock-registers
                                       phase2-local-registers
                                       (cons (list-ref meta-function 2) env)
                                       global-ids-box jump-box onif-symbol-hash)))
           (append (%asm-fun-header (car id-fun) (cadr meta-function) (list-ref meta-function 3))
                   free-vars-expand
                   make-env-frame
                   (list `(CONS (R 0) (R 1) (R ,another-register));そのあたらしいフレームを持ってきた環境にCONS
                         `(SET! (R 1) (R ,another-register)))
                   (cons '(COMMENT "FUNCTION BODY") asm-body)))))

   (define (onif-new-asm/asm-fun functions use-registers register-offset
                                 lock-registers local-register env
                                 global-ids-box jump-box onif-symbol-hash)
     (append-map
       (lambda (id-fun)
         (let ((body (list-ref (cadr id-fun) 3)))
          (if (and (pair? body)
                   (onif-misc/local-set!-operator?  (car body) onif-symbol-hash))
            (%asm-fun-lset id-fun use-registers register-offset lock-registers
                           local-register env global-ids-box jump-box
                           onif-symbol-hash)
            (%asm-fun id-fun use-registers register-offset lock-registers
                      local-register env global-ids-box jump-box
                      onif-symbol-hash))))
                functions))))
