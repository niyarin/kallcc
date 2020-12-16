(include "./onif-symbol.scm")
(include "./onif-misc.scm")
(include "./onif-idebug.scm")

"
This phase rules.
<expression>:<lambda-expression>|<begin-expression>|<const-val>
<lambda-expression>:(lambda (symbol ...) <expression>)
<begin-expression>:(begin <expression 1> ... <expression N>)
<const-val>:<symbol>|<number>|<boolean>
"
;TODO:next-phase (lambda (x) x) => x

(define-library (onif cps)
   (import (scheme base) (scheme cxr) (scheme list)
           (onif misc) (onif idebug) (onif symbol)
           (scheme write);
           )
   (export onif-cps-conv)

   (begin
      (define (%osym x)
        (if (onif-symbol? x)
          (onif-symbol->symbol x)
          x))

      (define (%if-operator? operator onif-symbol-hash)
        (cond
          ((not (onif-symbol? operator)) #f)
          ((eq? (onif-misc/onif-symbol-hash-ref onif-symbol-hash 'if)
                operator))
          (else #f)))

      (define (%lambda-operator? operator onif-symbol-hash)
        (cond
          ((not (onif-symbol? operator)) #f)
          ((eq? (onif-misc/onif-symbol-hash-ref onif-symbol-hash 'lambda)
                operator))
          (else #f)))

      (define (%onif-not-have-continuation? expression onif-symbol-hash)
         "If expression is primitive or quote-expression or lambda-operator,
          this function returning (list expression) ,otherwise #f."
         (cond
           ((not (list? expression)) (list expression))
           ((null? expression) (list '()))
           ((onif-misc/quote-operator? (car expression) onif-symbol-hash)
            (list expression))
           ((%lambda-operator? (car expression) onif-symbol-hash)
            (let ((cps-symbol (onif-symbol)))
              `((,(onif-misc/onif-symbol-hash-ref onif-symbol-hash 'lambda)
                 ,(cons cps-symbol (cadr expression))
                 ,(%cps-conv (car (cddr expression))
                             #f
                             (list (%conv-stack-cell cps-symbol #f #f))
                             onif-symbol-hash)))))
           (else #f)))

     (define-record-type <conv-stack-cell>
         (%conv-stack-cell cont-symbol expression next)
         %conv-stack-cell?
         (cont-symbol %conv-stack-cell-csymbol %conv-stack-cell-set-csymbol!)
         (expression %conv-stack-cell-expression %conv-stack-cell-set-expression!)
         (next %conv-stack-cell-next %conv-stack-cell-next!))

     (define (%conv-stack-cell->list stack-cell)
       (list (%conv-stack-cell-csymbol stack-cell)
             (%conv-stack-cell-expression stack-cell)
             (%conv-stack-cell-next stack-cell)))

      (define (%cps-conv-begin scm-code stack onif-symbol-hash)
        (%cps-conv-frun scm-code stack onif-symbol-hash #t))

      (define (%begin-last-expression scm-code stack onif-symbol-hash)
        (let ((res-val (last scm-code));;begin式の最後の式
              (begin-cont-symbol (%conv-stack-cell-csymbol (car stack)))
              (next-expressions (%conv-stack-cell-expression (car stack))))
            (cond
              ((not next-expressions) (list begin-cont-symbol res-val))
              (else
                (map! (lambda (x) (if (eq? x begin-cont-symbol) res-val x))
                      next-expressions)
                (%cps-conv next-expressions #f (cdr stack) onif-symbol-hash)))))

      (define (%cps-conv-frun-loop scm-code res-top-cell res-cell
                                   stack onif-symbol-hash beg-flag)
        (let loop ((code scm-code))
         (cond
           ((and (null? code) beg-flag (last-pair scm-code))
            (display "ALL DONE!")(newline)
              ;begin式の返り値のための処理
              (%begin-last-expression scm-code stack onif-symbol-hash))
           ((null? code);;全引数見た。
            (cons* (cadr res-top-cell)
                   (if (%conv-stack-cell-expression (car stack))
                       `(,(onif-misc/onif-symbol-hash-ref
                               onif-symbol-hash 'lambda)
                            (,(%conv-stack-cell-csymbol (car stack)))
                            ,(%cps-conv (%conv-stack-cell-expression (car stack))
                                        (%conv-stack-cell-next (car stack))
                                        (cdr stack) onif-symbol-hash))
                        (%conv-stack-cell-csymbol (car stack)))
                   (cddr res-top-cell)))
           ((%onif-not-have-continuation? (car code) onif-symbol-hash)
            => (lambda (const-box)
                  (set-cdr! res-cell (cons (car const-box) '()))
                  (set! res-cell (cdr res-cell))
                  (loop (cdr code))))
           ((and (pair? code) (null? (cdr code)) beg-flag)
            (%cps-conv (car code) #f stack onif-symbol-hash))
           (else;;;継続がある
             (let ((new-sym (onif-symbol)))
               (set-cdr! res-cell (cons new-sym (cdr code)))
               (%cps-conv (car code)
                          #f
                          (cons (%conv-stack-cell new-sym
                                                  (cdr res-top-cell)
                                                  (cddr res-cell))
                                stack)
                          onif-symbol-hash))))))

      (define (%ls-head-cell&cell code next)
        (let* ((head-cell (list #f))
               (cell head-cell))
          (let loop ((code code))
            (if (eq? code next)
              (values head-cell cell)
              (begin
                (set-cdr! cell (list (car code)))
                (set! cell (cdr cell))
                (loop (cdr code)))))))

      (define (%cps-conv-frun-next scm-code next stack onif-symbol-hash . beg-flag)
         (let-values (((res-top-cell res-cell)
                       (%ls-head-cell&cell scm-code next)))
           (let ((beg-flag (if (null? beg-flag) #f (car beg-flag))))
          (%cps-conv-frun-loop next res-top-cell res-cell stack
                               onif-symbol-hash beg-flag))))

      (define (%cps-conv-frun scm-code stack onif-symbol-hash . beg-flag)
         "この中では、関数適用だけではなく、begin式の展開にも使う。そのときはbeg-flagが立っている"
         (let* ((res-top-cell (list #f '()))
                (res-cell res-top-cell)
                (beg-flag (if (null? beg-flag) #f (car beg-flag))))
          (%cps-conv-frun-loop scm-code res-top-cell res-cell stack onif-symbol-hash beg-flag)))

      (define (%cps-conv-if scm-code stack onif-symbol-hash)
         (let ((test-const-exp
                 (%onif-not-have-continuation? (cadr scm-code) onif-symbol-hash))
               (if-symbol (car scm-code))
               (true-exp (list-ref scm-code 2))
               (false-exp (list-ref scm-code 3)))
           (if test-const-exp
             (list if-symbol
                   (car test-const-exp)
                   (%cps-conv true-exp #f stack onif-symbol-hash)
                   (%cps-conv false-exp #f stack onif-symbol-hash))
             (let ((new-sym (onif-symbol)))
                 (%cps-conv
                  (cadr scm-code)
                  #f
                  (cons (%conv-stack-cell
                          new-sym
                          `(,if-symbol ,new-sym ,true-exp ,false-exp)
                          #f)
                        stack)
                  onif-symbol-hash)))))

      (define (%cps-conv scm-code have-next stack onif-symbol-hash)
        (let ((const-val
                (%onif-not-have-continuation? scm-code onif-symbol-hash)))
           (cond
              ((and (not (null? stack))
                    (not (%conv-stack-cell-expression (car stack)))
                    const-val) =>
               (lambda (box-const-val)
                 (list (%conv-stack-cell-csymbol (car stack))
                       (car box-const-val))))
              (const-val => car)
              ((%if-operator?  (car scm-code) onif-symbol-hash)
                  (%cps-conv-if scm-code stack onif-symbol-hash))
              ((onif-misc/begin-operator? (car scm-code) onif-symbol-hash)
               ;nextの扱い
                  (%cps-conv-begin scm-code stack onif-symbol-hash))
              (have-next
                (%cps-conv-frun-next scm-code have-next stack onif-symbol-hash))
              (else (%cps-conv-frun scm-code stack onif-symbol-hash)))))

      (define (onif-cps-conv scm-code onif-symbol-hash)
        (if (boolean? scm-code);;あとで、ATOM全体に変えとく(定数はcps変換しない)
          scm-code
          (%cps-conv scm-code #f (list (%conv-stack-cell '() #f #f))
                     onif-symbol-hash)))))
