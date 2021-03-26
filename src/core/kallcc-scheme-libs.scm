(define-library (kallcc scm libs)
  (import (scheme base)
          (scheme hash-table)
          (scheme comparator)
          (prefix (kallcc symbol) ksymbol/))
  (export (make-kallcc-core-pair))
  (begin

    (define (%kallcc-core-pair-alist)
      `((car . (built-in-car ,(ksymbol/kallcc-symbol 'CAR)))
        (cdr . (built-in-cdr ,(ksymbol/kallcc-symbol 'CDR)))
        (set-car! . (built-in-car ,(ksymbol/kallcc-symbol 'SET-CAR!)))
        (set-cdr! . (built-in-car ,(ksymbol/kallcc-symbol 'SET-CDR!)))
        (cons . (built-in-cons ,(ksymbol/kallcc-symbol 'CONS)))))

    (define (%kallcc-core-syntax-alist)
      `((lambda . (built-in-lambda ,(ksymbol/kallcc-symbol 'LAMBDA)))
        (if . (built-in-if ,(ksymbol/kallcc-symbol 'IF)))
        (define . (built-in-define  ,(ksymbol/kallcc-symbol 'DEFINE)))
        (set! . (built-in-set! ,(ksymbol/kallcc-symbol 'SET!)))
        (begin . (built-in-begin ,(ksymbol/kallcc-symbol 'BEGIN)))))

    (define make-kallcc-core-pair #f)

    (define (%make-kallcc-core-pair)
      (alist->hash-table
        (%kallcc-core-pair-alist)
        (make-eq-comparator)))

    (let ((kallcc-core-pair* #f))
      (set! make-kallcc-core-pair
            (lambda ()
              (unless kallcc-core-pair* (set! kallcc-core-pair* (%make-kallcc-core-pair)))
              kallcc-core-pair*)))

    (define make-kallcc-core-syntax #f)

    (define (%make-kallcc-core-syntax)
      (alist->hash-table
        (%kallcc-core-syntax-alist)
        (make-eq-comparator)))

    (let ((kallcc-core-syntax* #f))
          (set! make-kallcc-core-syntax
                (lambda ()
                  (unless kallcc-core-syntax* (set! kallcc-core-syntax* (%make-kallcc-core-syntax)))
                  kallcc-core-syntax*)))))
