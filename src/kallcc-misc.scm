(define-library (kallcc misc)
  (import (scheme base)
          (scheme comparator)
          (scheme set)
          (prefix (kallcc symbol) ksymbol/))
  (export kerror make-tconc tconc-head tconc-push! scm-expression->symbol-set
          rename-symbol-in-expression)
  (begin
    (define kerror error)

    (define (var? obj)
      (or (symbol? obj)
          (ksymbol/kallcc-symbol? obj)))

    (define-record-type <tconc>
      (%make-tconc head tail)
      tconc?
      (head %tconc-head %tconc-set-head!)
      (tail %tconc-tail %tconc-set-tail!))

    (define (make-tconc)
      (let ((cell (list #f)))
        (%make-tconc cell cell)))

    (define (tconc-head obj)
      (cdr (%tconc-head obj)))

    (define (tconc-push! tconc x)
       (set-cdr! (%tconc-tail tconc) (list x))
       (%tconc-set-tail! tconc (cdr (%tconc-tail tconc))))

    (define (%scm-expression->symbol-list expression)
      (let loop ((expression expression))
        (cond
          ((pair? expression)
           (append (%scm-expression->symbol-list (car expression))
                   (%scm-expression->symbol-list (cdr expression))))
          ((var? expression) (list expression))
          (else '()))))

    (define (scm-expression->symbol-set expression)
      (list->set (make-eq-comparator)
                 (%scm-expression->symbol-list expression)))

    (define (rename-symbol-in-expression expression alist)
      (let loop ((expression expression))
        (cond
          ((pair? expression)
           (cons (loop (car expression))
                 (loop (cdr expression))))
          ((and (symbol? expression)
                (assq expression alist))
           => cdr)
          (else expression))))
    ))
