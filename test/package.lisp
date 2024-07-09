;;;
;;; Copyright (c) 2016 - 2022 Paul Onions
;;; Licence: MIT, see LICENCE file for details
;;;
;;; Test package definition for the SB-TCL system.
;;;
(defpackage :sb-tcl-test
  (:use :common-lisp :sb-tcl-test/simple-test)
  (:export
   :tcl-test :tk-test))

