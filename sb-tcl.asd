;;;
;;; Copyright (c) 2016 Paul Onions
;;; Licence: MIT, see LICENCE file for details
;;;
;;; ASDF system definition for the SB-TCL system.
;;;
(in-package :cl-user)

(asdf:defsystem :sb-tcl
  :author "Paul Onions"
  :version "0.0.0"
  :licence "MIT"
  :description "SB-TCL: a low-level connection from SBCL to Tcl."
  :depends-on ()
  :components ((:file "packages")
               (:file "conditions")
               (:file "aliens")
               (:file "interface"))
  :serial t)
