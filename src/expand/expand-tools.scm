(define-library (kallcc expand tools)
  (import (scheme base) (scheme list)
          (prefix (kallcc misc) kmisc/))
  (export reference-global-variabls)
  (begin
    (define (%reference-global-variabls* expanded-expression global)
      ;;ひとまずsimple実装
      (let loop ((expression expanded-expression))
        (cond
          ((pair? expression) (append (loop (car expression)) (loop (cdr expression))))
          ((kmisc/var? expression) (list expression))
          (else '()))))

    (define (reference-global-variabls expanded-expression global)
      (delete-duplicates (%reference-global-variabls* expanded-expression global)))))
