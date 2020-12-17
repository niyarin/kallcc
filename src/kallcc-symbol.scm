(define-library (kallcc symbol)
   (import (scheme base)
           (onif symbol))
   (export kallcc-symbol kallcc-symbol?)
   (begin
     (define kallcc-symbol onif-symbol)
     ;(define kallcc-symbol? onif-symbol?)
     (define kallcc-symbol? (lambda (x) onif-symbol? x))))
