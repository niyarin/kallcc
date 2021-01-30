(define-library (kallcc regflat)
  (import (scheme base)
          (scheme write);
          (scheme hash-table);
          (onif idebug);
          (prefix (kallcc misc) kmisc/))
  (export regflatable? regflat)
  (begin
    (define (regflatable? code) #t)

    (define (regflat code rev-symbol-hash)
      (if (pair? code)
        (let ((operator (car code)))
         (onif-idebug/debug-display (list operator (kmisc/operator->symbol operator symbol-hash)))(newline)
         (display (hash-table-contains? symbol-hash operator))

         (display (hash-table-keys symbol-hash))
         (newline)
          code)
        code))))
