(define-library (kallcc symbol)
   (import (scheme base)
           (onif symbol))
   (export kallcc-symbol kallcc-symbol? ref-symbol)
   (begin
     (define kallcc-symbol onif-symbol)
     (define kallcc-symbol? onif-symbol?)

     (define ref-symbol onif-symbol/ref-symbol)))
