(define-library (kallcc misc)
  (import (scheme base))
  (export kerror make-tconc tconc-head tconc-push!)
  (begin
    (define kerror error)

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
       (%tconc-set-tail! tconc (cdr (%tconc-tail tconc))))))
