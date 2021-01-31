(define-library (kallcc misc)
  (import (scheme base)
          (scheme comparator)
          (scheme set)
          (scheme hash-table)
          (onif symbol)
          (prefix (kallcc symbol) ksymbol/))
  (export kerror make-tconc tconc-head tconc-push! scm-expression->symbol-set find-in-expression
          rename-symbol-in-expression inverse-alist var?
          formals->list list-update rassq assq-cdr-default assq-cadr-default)
  (begin
    (define kerror error)

    (define (var? obj)
      (or (symbol? obj)
          (onif-symbol? obj)
          ;(ksymbol/kallcc-symbol? obj)
          ))

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

    (define (find-in-expression expression proc)
      ;;木から述語を満たすものを取り出す
      (let loop ((expression expression))
        (cond
          ((proc expression) (list expression))
          ((pair? expression)
           (append (loop (car expression))
                   (loop (cdr expression))))
          (else '()))))

    (define (scm-expression->symbol-set expression . symbol-procedure-opt)
      (let ((symbol-procedure (if (not (null? symbol-procedure-opt))
                                 (car symbol-procedure-opt)
                                 var?)))
        (list->set (make-eq-comparator)
                   (find-in-expression expression symbol-procedure))))

    (define (inverse-alist alist)
      (map (lambda (apair) (cons (cdr apair) (car apair)))
           alist))

    (define (rename-symbol-in-expression expression alist)
      (let loop ((expression expression))
        (cond
          ((pair? expression)
           (cons (loop (car expression))
                 (loop (cdr expression))))
          ((and (var? expression)
                (assq expression alist))
           => cdr)
          (else expression))))

    (define (formals->list formals)
       (let loop ((fs formals))
         (cond
           ((pair? fs)
            (cons (car fs)
                  (loop (cdr fs))))
           ((null? fs) '())
           (else (list fs)))))

    (define (list-update ls index val)
      (let loop ((i 0)
                 (ls ls))
        (if (= i index)
          (cons val
                (cdr ls))
          (cons (car ls) (loop (+ i 1) (cdr ls))))))

    (define (rassq val als);;cadr
      (let loop ((als als))
        (cond
          ((null? als) #f)
          ((eq? (cadr (car als)) val) (car als))
          (else (loop (cdr als))))))

    (define (assq-cdr-default key alist default)
      (cond ((assq key alist) => cdr) (else default)))

    (define (assq-cadr-default key alist default)
      (cond ((assq key alist) => cdr) (else default)))))
