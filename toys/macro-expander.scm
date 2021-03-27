(include "../src/core/includes.scm")
(include "../src/onif-idebug.scm")
(include "../src/expand/includes.scm")

(import (scheme base) (scheme write) (scheme read) (scheme file)
        (scheme process-context) (scheme list)

        (niyarin thread-syntax)
        (onif scm env)
        (prefix (kallcc expand library) ke-library/)
        (prefix (kallcc expand expand) ke-expand/)
        (prefix (kallcc expand preprocess) ke-preprocess/))

(define (expand libraries)
  (->> libraries
      (map (lambda (library)
             (if (ke-expand/expandable-library? library '())
               (display "OK\n")
               (display "NG\n")))
       )))

(define (include1 filename)
  (ke-preprocess/preprocess (read-file filename)))

(define (library-parts)
  `())


(define (read-file filename)
  (call-with-input-file filename
      (lambda (input-port)
        (do ((res '() (cons (read input-port) res)))
            ((eof-object? (peek-char input-port)) (reverse (cdr res)))))))

(define (main*)
  (let* ((args (cdr (command-line)))
         (syntax-libs
           (ke-preprocess/preprocess (append (read-file "../ref-liv/syntax1.scm")
                                             (read-file "../ref-liv/scheme-base.scm"))))
         (code (ke-preprocess/preprocess
                 (cons '(import (scheme base)) (read-file (car args))))))
        (display (car syntax-libs))(newline)
    (display (expand (append syntax-libs code))) (newline)))

(main*)
