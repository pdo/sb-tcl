;;;
;;; Copyright (c) 2016 - 2022 Paul Onions
;;; Licence: MIT, see LICENCE file for details
;;;
;;; ASDF system definition for the SB-TCL test system.
;;;
(in-package :cl-user)

(asdf:defsystem :sb-tcl-test
  :author "Paul Onions"
  :version "0.1"
  :licence "MIT"
  :description "SB-TCL-TEST: test system for SB-TCL."
  :depends-on (:sb-tcl)
  :components ((:file "simple-test")
               (:file "package"))
  :serial t)
