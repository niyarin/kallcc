(include "./onif-symbol.scm")
(include "./lib/thread-syntax.scm")

(define-library (onif alpha conv)
   (import (scheme base)
           (onif symbol)
           (prefix (kallcc misc) kmisc/)
           (scheme list)
           (srfi 125) ;SCHEME HASH
           (srfi 127)
           (onif misc)
           (only (niyarin thread-syntax) ->>))
   (export onif-alpha/conv!
           onif-alpha/simple-conv!)
   (begin
      (define (%onif-symbol sym onif-symbol-hash)
        (cond
          ((hash-table-ref onif-symbol-hash sym) => cadr)
          (else #f)))

     (define (%lambda-operator? operator onif-symbol-hash)
        (cond
          ((not (onif-symbol? operator)) #f)
          ((eq? (cadr (hash-table-ref onif-symbol-hash 'lambda))
                operator))
          (else #f)))

     (define (%lookup-stack symbol stk)
       (let ((res (->> stk
                       (lseq-map (lambda (frame-alist) (assv symbol frame-alist)))
                       (lseq-filter (lambda (x) x)))))
         (and (not (null? res)) (lseq-car res))))

     (define (%symbol-conv symbol stk)
       (cond
         ((%lookup-stack symbol stk)
          => cdr)
         (else symbol)))

     (define (onif-alpha/simple-conv! code conv-table onif-symbol-hash)
       (let loop ((code code))
         (cond
           ((and (symbol? code)
                 (assq code conv-table))
            => cadr)
           ((not-pair? code) code)
           ((onif-misc/quote-operator? (car code) onif-symbol-hash) code)
           (else (map! (lambda (x) (loop x)) code)))))

     (define (%alpha-conv-ref-var sym-info)
       (let ((type (car sym-info))
             (symbol (cadr sym-info)))
         (cond
           ((eq? type 'global) symbol)
           (else (error "!!" sym-info)))))

     (define (%make-stack-cell formals current-stack)
       (map (lambda (x)
               (if (%lookup-stack x current-stack)
                 (cons x (onif-symbol x))
                 (cons x x)))
            (kmisc/formals->list formals)))

     (define (onif-alpha/conv! code onif-symbol-hash
                               . optional-inital-stack)
       (let ((initial-stk
               (cond
                   ((null? optional-inital-stack) '())
                   ((list? (car optional-inital-stack))
                    (car optional-inital-stack))
                   (else
                     (error "optional-inital-stack must be list")))))
         (begin
             (let loop ((code code)
                        (stk initial-stk))
               (cond
                 ((kmisc/var? code) (%symbol-conv code stk))
                 ((not (list? code)) code)
                 ((null? code) '())
                 ((onif-misc/ref-var-operator? (car code) onif-symbol-hash) (%alpha-conv-ref-var (cadr code)))
                 ((onif-misc/quote-operator? (car code) onif-symbol-hash) code)
                 ((%lambda-operator? (car code) onif-symbol-hash)
                  (let ((stack-cell (%make-stack-cell (cadr code) stk)))
                    (begin
                      ;;rename-formal
                      (set-car! (cdr code)
                                (kmisc/rename-symbol-in-expression
                                  (cadr code)
                                  stack-cell))
                      ;;rename body
                      (set-cdr! (cdr code)
                                (map (lambda (body)
                                        (loop body (cons stack-cell stk)))
                                     (cddr code)))
                      code)))
                 (else
                   (pair-for-each
                     (lambda (cell) (set-car!  cell (loop (car cell) stk)))
                     code)
                   code)))

            (cond
              ((and (list? code)
                    (onif-misc/define-operator? (car code) onif-symbol-hash)
                    (%lookup-stack (cadr code) initial-stk))
               => (lambda (renamed-symbol) (set-car! (cdr code) renamed-symbol)))
              (else)))))))
