(defpackage #:printf.tests
  (:use #:cl #:printf)
  (:export #:run-tests))

(in-package #:printf.tests)

(defmacro assert-equal (expr-1 expr-2)
  (let ((val-1-sym (gensym "VAL-1"))
        (val-2-sym (gensym "VAL-2")))
    `(let ((,val-1-sym ,expr-1)
           (,val-2-sym ,expr-2))
       (unless (equal ,val-1-sym ,val-2-sym)
         (format *error-output* "FAILURE to match~%  ~s => ~s~%  ~s => ~s~%"
                 ',expr-1 ,val-1-sym ',expr-2 ,val-2-sym)))))

(defun run-tests ()
  (assert-equal (sprintf "% d" 1) " 1")
  (assert-equal (sprintf "%d" -8589934591) "-8589934591")
  (assert-equal (sprintf "%+8d" 100) "    +100")
  (assert-equal (sprintf "%+.8d" 100) "+00000100")
  (assert-equal (sprintf "%-1.5d" -100) "-00100")
  (assert-equal (sprintf "%5d" 100) "  100")
  (assert-equal (sprintf "%5d" -100) " -100")
  (assert-equal (sprintf "%-5d" 100) "100  ")
  (assert-equal (sprintf "%-5d" -100) "-100 ")
  (assert-equal (sprintf "%-.5d" 100) "00100")
  (assert-equal (sprintf "%-.5d" -100) "-00100")
  (assert-equal (sprintf "%-8.5d" 100) "00100   ")
  (assert-equal (sprintf "%-8.5d" -100) "-00100  ")
  (assert-equal (sprintf "%05d" 100) "00100")
  (assert-equal (sprintf "%05d" -100) "-0100")
  (assert-equal (sprintf "% d" 100) " 100")
  (assert-equal (sprintf "% d" -100) "-100")
  (assert-equal (sprintf "% 5d" 100) "  100")
  (assert-equal (sprintf "% 5d" -100) " -100")
  (assert-equal (sprintf "% .5d" 100) " 00100")
  (assert-equal (sprintf "% .5d" -100) "-00100")
  (assert-equal (sprintf "% 8.5d" 100) "   00100")
  (assert-equal (sprintf "% 8.5d" -100) "  -00100")
  (assert-equal (sprintf "%.0d" 0) "")
  (assert-equal (sprintf "%#-8.5o" 100) "00144   ")
  (assert-equal (sprintf "%#-+ 08.5d" 100) "+00100  ")
  (assert-equal (sprintf "%#-+ 08.5d" 100) "+00100  ")
  (assert-equal (length (sprintf "%.80d" 100)) 80)
  (assert-equal (length (sprintf "% .80d" 100)) 81)

  (assert-equal (sprintf "% d" 1) " 1")
  (assert-equal (sprintf "%+ d" 1) "+1")
  (assert-equal (sprintf "%04c" #\1) "0001")
  (assert-equal (sprintf "%-04c" #\1) "1   ")
  (assert-equal (sprintf "%04s" "foo") "0foo")
  (assert-equal (sprintf "%.1s" "foo") "f")
  (assert-equal (sprintf "%.*s" 1 "foo") "f")
  (assert-equal (sprintf "%*s" -5 "foo") "foo")
  (assert-equal (sprintf "% .5d" 100) " 00100")
  (assert-equal (sprintf "%.0d" 0) ""))