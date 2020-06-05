(include "./onif-symbol.scm")
(include "./lib/thread-syntax.scm")

(define-library (onif alpha conv)
   (import (scheme base)
           (onif symbol)
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
         (if (null? res)
           #f
           (lseq-car res))))

     ;memorize
     (define (%symbol-conv symbol stk)
       (cond
         ((%lookup-stack symbol stk)
          => (lambda (x)
               (lseq-car (lseq-cdr x))))
         (else symbol)))

     (define (onif-alpha/simple-conv! code conv-table onif-symbol-hash)
       (let loop ((code code))
         (cond
           ((and (symbol? code)
                 (assq code conv-table))
            => cadr)
           ((not-pair? code) code)
           ((onif-misc/quote-operator? (car code) onif-symbol-hash) code)
           (else
             (begin
                (->> code
                     (onif-misc/for-each-cell1
                       (lambda (cell)
                         (set-car! cell
                                   (loop (car cell))))))
                code)))))

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
                 ((symbol? code)
                  (%symbol-conv code stk))
                 ((not (list? code)) code)
                 ((null? code) '())
                 ((onif-misc/quote-operator? (car code) onif-symbol-hash) code)
                 ((%lambda-operator? (car code) onif-symbol-hash)
                  (let* ((formals (cadr code))
                         (stack-cell
                             (map
                               (lambda (x)
                                  (let ((conflicted (%lookup-stack x stk)))
                                    (if conflicted
                                       (list x (onif-symbol x))
                                       (list x x))))
                               formals)))
                    (begin
                       (set-car!  (cdr code) (map cadr stack-cell))
                       (set-cdr!  (cdr code)
                                  (map
                                     (lambda (body)
                                        (loop body (cons stack-cell stk)))
                                     (cddr code)))
                       code)))
                 (else
                   (onif-misc/for-each-cell1
                     (lambda (cell)
                        (set-car!
                           cell
                           (loop (car cell) stk)))
                     code)
                   code)))

            (cond
              ((and (onif-misc/define-operator? (car code) onif-symbol-hash)
                    (%lookup-stack (cadr code) initial-stk))
               => (lambda (renamed-symbol) (set-car! (cdr code) renamed-symbol)))
              (else)))))))
