(define-library (onif my-specs)
   (import (scheme base)
           (scmspec core))
   (export my-specs/libname? my-specs/scheme-expression? my-specs/import-expression?)
   (begin
     (define my-specs/libname?  (scmspec/list-of symbol?))

     (define my-specs/scheme-expression? scmspec/any?)

     (define my-specs/import-expression?
       (scmspec/pair (scmspec/eq 'import)
                     scmspec/any?))))
