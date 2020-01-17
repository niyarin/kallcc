(include "./onif-symbol.scm")
(include "./onif-misc.scm")

(define-library (onif cps)
   (import (scheme base)
           (scheme cxr)
           (onif misc)
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
           ((eq? (car expression) 'quote);QUOTE
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

      (define (%cps-conv-frun scm-code stack onif-symbol-hash)
         (let* ((res-top-cell (list #f '()))
                (res-cell res-top-cell))
             (let loop ((code scm-code))
                 (cond
                   ((null? code)
                    (cons
                      (car scm-code)
                      (cons
                        (if (cadar stack)
                          `(lambda (,(caar stack))
                                   ,(%cps-conv (cadar stack) (cdr stack) onif-symbol-hash))
                           (caar stack))
                          (cdr scm-code))))
                   ((%onif-not-have-continuation?
                      (car code)
                      onif-symbol-hash)
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
                           (list new-sym (cdr res-top-cell) )
                           stack)
                         onif-symbol-hash)))))))

      (define (%cps-conv-if scm-code stack onif-symbol-hash)
         (let ((test-const-exp
                 (%onif-not-have-continuation? (cadr scm-code) onif-symbol-hash)))
           (if test-const-exp
             (list
                   (car scm-code)
                   (car test-const-exp)
                   (%cps-conv (list-ref scm-code 2) stack onif-symbol-hash)
                   (%cps-conv (list-ref scm-code 3) stack onif-symbol-hash))
             (%cps-conv
               (let ((new-sym (onif-symbol)))
                 (%cps-conv
                  (cadr scm-code)
                  (cons
                    (list new-sym
                          (list (car code)
                                new-sym
                                (list-ref code 2)
                                (list-ref code 3)))
                    stack)
                  onif-symbol-hash))))))

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
             (else
               (%cps-conv-frun scm-code stack onif-symbol-hash)))))

      (define (onif-cps-conv scm-code onif-symbol-hash)
        (%cps-conv scm-code '() onif-symbol-hash))))
