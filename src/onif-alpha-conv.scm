(include "./onif-symbol.scm")
(include "./onif-utils.scm")

(define-library (onif alpha conv)
   (import (scheme base)
           (onif symbol)
           (srfi 125) ;SCHEME HASH
           (srfi 127)
           (onif utils))
   (export onif-alpha/conv!)
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
       (let ((res
                   (lseq-filter
                     (lambda (x) (not (null? x)))
                      (lseq-map
                        (lambda (s)
                          (assv symbol s))
                        stk))))
         (if (null? res)
           #f
           (lseq-car res))))

     (define (%symbol-conv symbol stk)
       (cond
         ((%lookup-stack symbol stk)
          => (lambda (x)
               (lseq-car (lseq-cdr x))))
         (else symbol)))

     (define (onif-alpha/conv! code onif-symbol-hash
                               . optional-inital-stack)
       (let loop ((code code)
                  (stk (cond
                         ((null? optional-inital-stack) '())
                         ((list? (car optional-inital-stack))
                          (car optional-inital-stack))
                         (else
                           (error "optional-inital-stack must be list")))))
         (cond
           ((symbol? code)
            (%symbol-conv code stk))
           ((not (list? code)) code)
           ((null? code) '())
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
                 (set-car!
                   (cdr code)
                   (map cadr stack-cell))

                 (set-cdr!
                   (cdr code)
                    (map
                      (lambda (body)
                         (loop body (cons stack-cell stk)))
                      (cddr code)))
                 code)))
           (else
             (onif-utils-for-each-cell1
               (lambda (cell)
                  (set-car!
                     cell
                     (loop (car cell) stk)))
               code)
             code))))))
