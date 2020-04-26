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
   (import (scheme base)
           (scheme cxr)
           (scheme write);
           (onif misc)
           (onif idebug)
           (srfi 1);SCHEME LIST
           (onif symbol))
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
         (cond
           ((not (list? expression));ATOM(AND NOT NULL)
               (list expression))
           ((null? expression);NULL
               (list '()))
           ((onif-misc/quote-operator? (car expression) onif-symbol-hash)
            (list expression))
           ((%lambda-operator? (car expression) onif-symbol-hash);LAMBDA
            (let ((cps-symbol (onif-symbol)))
              (list
                 (list
                   (onif-misc/onif-symbol-hash-ref onif-symbol-hash 'lambda)
                   (cons cps-symbol (cadr expression))
                  (%cps-conv
                    (car (cddr expression))
                    `((,cps-symbol #f))
                    onif-symbol-hash
                    )))))
           (else
             #f)))

      (define (%cps-conv-begin scm-code stack onif-symbol-hash)
        (%cps-conv-frun scm-code stack onif-symbol-hash #t))

      (define (%stack-symbol-update! stack symbol)
        (let ((target (car stack)))
         (set-car! (car stack) symbol)
         (set-cdr! (car stack)
            (map (lambda (x)
                   (if (eq? x target)
                     symbol
                     x))
                 (cdar stack)))))

      (define (%begin-last-expression scm-code stack onif-symbol-hash)
        (let ((res-val (last scm-code))
              (begin-cont-symbol (caar stack)))
            (cond
              ((not (cadar stack)) res-val)
              (else
                (map! (lambda (x)
                        (if (eq? x begin-cont-symbol)
                          res-val
                          x))
                      (cadar stack))))))

      (define (%cps-conv-frun scm-code stack onif-symbol-hash . beg-flag)
         (let* ((res-top-cell (list #f '()))
                (res-cell res-top-cell)
                (beg-flag (if (null? beg-flag) #f (car beg-flag))))
             (let loop ((code scm-code))
                 (cond
                   ((and (null? code) beg-flag (last-pair scm-code))
                      (%begin-last-expression scm-code stack onif-symbol-hash))
                   ((null? code)
                    (cons (cadr res-top-cell)
                          (cons (if (cadar stack)
                                  `(,(onif-misc/onif-symbol-hash-ref
                                       onif-symbol-hash 'lambda)
                                    (,(caar stack))
                                    ,(%cps-conv (cadar stack)
                                                (cdr stack) onif-symbol-hash))
                                  (caar stack))
                              (cddr res-top-cell))))
                   ((%onif-not-have-continuation? (car code) onif-symbol-hash)
                    => (lambda (cont-val)
                          (set-cdr! res-cell (cons (car cont-val) '()))
                          (set! res-cell (cdr res-cell))
                          (loop (cdr code))))
                   (else
                     (let ((new-sym (onif-symbol)))
                       (set-cdr! res-cell (cons new-sym (cdr code)))
                       (%cps-conv
                         (car code)
                         (cons
                           (list new-sym (cdr res-top-cell))
                           stack)
                         onif-symbol-hash)))))))

      (define (%cps-conv-if scm-code stack onif-symbol-hash)
         (let ((test-const-exp
                 (%onif-not-have-continuation? (cadr scm-code) onif-symbol-hash)))
           (if test-const-exp
             (list (car scm-code)
                   (car test-const-exp)
                   (%cps-conv (list-ref scm-code 2) stack onif-symbol-hash)
                   (%cps-conv (list-ref scm-code 3) stack onif-symbol-hash))
             (let ((new-sym (onif-symbol)))
                 (%cps-conv
                  (cadr scm-code)
                  (cons
                    (list new-sym
                          (list (car scm-code)
                                new-sym
                                (list-ref scm-code 2)
                                (list-ref scm-code 3)))
                    stack)
                  onif-symbol-hash)))))

      (define (%cps-conv scm-code stack onif-symbol-hash)
        (let ((const-val (%onif-not-have-continuation?
                           scm-code
                           onif-symbol-hash)))
           (cond
              ((and (not (null? stack))
                    (not (cadar stack))
                    const-val) =>
               (lambda (box-const-val)
                 (list (caar stack) (car box-const-val))))
              (const-val => car)
              ((%if-operator?  (car scm-code) onif-symbol-hash)
                  (%cps-conv-if scm-code stack onif-symbol-hash))
              ((onif-misc/begin-operator? (car scm-code) onif-symbol-hash)
                  (%cps-conv-begin scm-code stack onif-symbol-hash))
              (else
                  (%cps-conv-frun scm-code stack onif-symbol-hash)))))

      (define (onif-cps-conv scm-code onif-symbol-hash)
        (%cps-conv scm-code '((() #f)) onif-symbol-hash))))
