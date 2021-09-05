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
          boot-virtual-lib)
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
                           (export-rules (ke-library/lassq 'export target-lib)));;see also ke-preprocess/library-add-export-keys
                      ;;exportの処理がまだ
                      ;;renameと対象のフィルタリング
                      ;;virtual-libsはそのままでよくないか?
                      ;;いや、この手続きは、boot-virtual-lib以外用にも使うから、exportがなければそのままloadし、
                      ;;exportがあれば、それを適用する
                      (%import-set->env-alist import-set env))));;<= import内のonly except prefix renameを処理する
             concatenate)
        (make-eq-comparator)))

    (define (boot-virtual-lib virtual-libraries libraries)
      ;;scheme-base.scmなどの処理系内部のライブラリの展開準備など
      (map (lambda (library);;boot scm-lib core
             (if (expandable-library? library virtual-libraries)
               (let ((env (make-expand-environment (ke-library/lassq 'import-libraries library) virtual-libraries)))
                 (begin (display "OK:") (display (car library))(newline))
                 ;;環境を確定できる。
                 (ke-library/update library 'env env))
               (begin (display "NG:") (display (car library))(newline) library)))
           libraries))

    ;(define (boot-primary-lib libraries)
    ;  "boot-virtual-libで展開可能とされたライブラリを使って展開可能なライブラリにenvをセットする"
    ;  (let ((primary-libs (filter (lambda (x) (ke-library/lassq 'env library #f)))))
    ;  ))

    (define (expand-onece expression env ignore-expressions macro-expand-step)
      (cond
        ((and (pair? scm-code) (kmisc/var? (car scm-code))))
        ((pair? scm-code))
        (else scm-code)))

    (define (expand expression env ignore-expressions)
      ;;ignore-expressionsは、展開結果を見るだけのとき、let等の展開をさせないため
      (expand-onece expression env ignore-expressions '()))))
