(define-library (kallcc expand expand)
  (import (scheme base) (scheme list) (scheme hash-table) (scheme comparator)
          (scheme write);;

          (niyarin thread-syntax)
          (prefix (niyarin preder) p/)

          (prefix (kallcc symbol) ksymbol/)
          (prefix (kallcc misc) kmisc/)
          (prefix (kallcc expand preprocess) ke-preprocess/)
          (prefix (kallcc expand library) ke-library/))
  (export expandable-library? make-expand-environment
          boot-virtual-lib
          boot-primary-lib

          )
  (begin
    (define (expandable-library? library libraries)
      ;すべてが展開可能libraryなものなら#trueを返す
      (every (lambda (libname) (any (p/equal? libname) (map car libraries)))
             (ke-library/lassq 'import-name-only library)))

    (define (%import-set->env-alist import-set env)
      (case (car import-set)
       ((only))
       ((except))
       ((prefix))
       ((rename))
       (else (hash-table->alist env))))

    (define (make-expand-environment import-expression libraries)
      (alist->hash-table
        (->> import-expression
             (map (lambda (import-set)
                    (let* ((target-lib (assoc (ke-preprocess/import-set->libname import-set) libraries))
                           (env (ke-library/lassq 'env target-lib))
                           (export-rules (ke-library/lassq 'export target-lib #f)));;see also ke-preprocess/library-add-export-keys
                      (%import-set->env-alist import-set env))));;<= import内のonly except prefix renameを処理する
             concatenate)
        (make-eq-comparator)))

    (define (boot-virtual-lib virtual-libraries libraries)
      ;;scheme-base.scmなどの処理系内部のライブラリの展開準備など
      (map (lambda (library);;boot scm-lib core
             (if (expandable-library? library virtual-libraries)
               (let* ((env (make-expand-environment (ke-library/lassq 'import-libraries library) virtual-libraries)))
                 (begin (display "OK:") (display (car library))(newline))
                 ;;環境を確定できる。
                 (ke-library/update library 'env env))
               (begin (display "NG:") (display (car library))(newline) library)))
           libraries))

    (define (boot-primary-lib virtual-libraries libraries)
    ;  "boot-virtual-libで展開可能とされたライブラリを使って展開可能なライブラリにenvをセットする"
      (let-values (((primary-libs target-libraries) (partition (lambda (library) (ke-library/lassq 'env library #f)) libraries)))
        (append primary-libs
                (map (lambda (library)
                      (if (expandable-library? library (append primary-libs virtual-libraries))
                        (let* ((env (make-expand-environment (ke-library/lassq 'import-libraries library)
                                                             (append virtual-libraries primary-libs))))
                          (begin (display "OK") (display (car library)) (newline))
                          (ke-library/update library 'env env))
                        (begin (display "NG")
                               (display (car library))
                               (newline)
                               library)))
                    target-libraries))))

    (define (expand-onece expression env ignore-expressions macro-expand-step)
      (cond
        ((and (pair? scm-code) (kmisc/var? (car scm-code))))
        ((pair? scm-code))
        (else scm-code)))

    (define (expand expression env ignore-expressions)
      ;;ignore-expressionsは、展開結果を見るだけのとき、let等の展開をさせないため
      (expand-onece expression env ignore-expressions '()))))
