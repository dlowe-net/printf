;;;; package.lisp

(defpackage #:printf
  (:use #:cl #:smug)
  (:export #:vfprintf #:fprintf #:vsprintf #:sprintf #:vprintf #:printf))
