;;;
;;; Copyright (c) 2016 - 2022 Paul Onions
;;; Licence: MIT, see LICENCE file for details
;;;
;;; Some simple tests/examples of SB-TCL usage.
;;;
(defpackage :sb-tcl-test/simple-test
  (:use :common-lisp :sb-tcl)
  (:export
   :tcl-test :tk-test))

(in-package :sb-tcl-test/simple-test)

(define-tcl-callout start-repl-server (&optional (port 0)) integer
  (list "::sb-tcl::start_repl_server" port))

(define-tcl-command my_pow ((b integer) (e integer))
  (expt b e))

(define-tcl-callout powers-of-2 (exponents) (list integer)
  (list "lmap" "n" exponents "my_pow 2 $n"))

(defun tcl-test ()
  (start-tcl-interpreter)
  (format t "Tcl version: ")
  (force-output)
  (interpret-tcl "set tcl_version")
  (format t "~S~%" (get-tcl-result-as 'string))
  (force-output)
  (register-tcl-command 'my_pow)
  (format t "Powers of 2: ")
  (force-output)
  (format t "~S~%" (powers-of-2 '(1 2 3 4)))
  (force-output)
  (stop-tcl-interpreter))

(defparameter *tk-repl-script*
  (merge-pathnames "tk-repl.tcl" (or #.*compile-file-truename* *load-truename*)))

(defun tk-test ()
  (start-tcl-interpreter :with-tk t)
  (format t "Tcl REPL server on port ~A~%" (start-repl-server))
  (force-output)
  (interpret-tcl (format nil "source ~A" *tk-repl-script*))
  (interpret-tcl "openReplWindow {}")
  (enter-tk-main-loop)
  (stop-tcl-interpreter))
