;;;; package.lisp

(defpackage #:regex
  (:use #:cl)
  (:export #:scan))

(defpackage #:regex-test
  (:use #:cl #:regex #:fiveam))
