;;;
;;; Copyright (c) 2016 - 2020 Paul Onions
;;; Licence: MIT, see LICENCE file for details
;;;
;;; Some simple tests/examples of SB-TCL usage.
;;;
(in-package :sb-tcl-test)

(define-tcl-callout tclver () string
  "Return the Tcl version string."
  '("set" "tcl_version"))

(define-tcl-callout listme () (list integer)
  "Test parsing of list return type."
  '("list" "1" "2" "3"))

(define-tcl-command hello ((name string))
  (format nil "Hello, ~A!" name))

(defun tcl-test (&optional (name "world"))
  (start-tcl-interpreter)
  (format t "Tcl callout test: ")
  (force-output)
  (format t "Tcl version is ~A~%" (tclver))
  (force-output)
  (format t "Tcl callout test: ")
  (force-output)
  (format t "Returned list is ~S~%" (listme))
  (force-output)
  (format t "Tcl command test: ")
  (force-output)
  (register-tcl-command 'hello)
  (let ((script (format nil "hello ~A" name)))
    (interpret-tcl script)
    (format t "~A~%" (get-tcl-result-as 'string)))
  (force-output)
  (stop-tcl-interpreter))

(defparameter *tk-repl-script*
  (merge-pathnames "tk-repl.tcl" (or #.*compile-file-truename* *load-truename*)))

(defun tk-test ()
  (start-tcl-interpreter :with-tk t)
  (interpret-tcl (format nil "source ~A" *tk-repl-script*))
  (interpret-tcl "openReplWindow {}")
  (enter-tk-main-loop)
  (stop-tcl-interpreter))
