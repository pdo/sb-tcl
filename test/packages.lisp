;;;
;;; Copyright (c) 2016 Paul Onions
;;; Licence: MIT, see LICENCE file for details
;;;
;;; Test package definition for the SB-TCL system.
;;;
(in-package :cl-user)

(defpackage :sb-tcl-test
  (:use :common-lisp :sb-tcl)
  (:export
   #:tcl-test #:tk-test))
