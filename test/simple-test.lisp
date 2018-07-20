;;;
;;; Copyright (c) 2016 - 2018 Paul Onions
;;; Licence: MIT, see LICENCE file for details
;;;
;;; Some simple tests/examples of SB-TCL usage.
;;;
(in-package :sb-tcl-test)

(define-tcl-callout tcl-version () string
  "Return the Tcl version string."
  '("set" "tcl_version"))

(define-tcl-callback hello ((name string))
  (format nil "Hello, ~A!" name))

(defun tcl-test (&optional (name "world"))
  (start-tcl-interpreter)
  (format t "Tcl command test: ")
  (force-output)
  (format t "Tcl version is ~A~%" (tcl-version))
  (force-output)
  (format t "Tcl callback test: ")
  (force-output)
  (register-tcl-callback 'hello)
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
