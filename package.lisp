;;;
;;; Copyright (c) 2016 - 2022 Paul Onions
;;; Licence: MIT, see LICENCE file for details
;;;
;;; Package definition for the SB-TCL system.
;;;
(defpackage :sb-tcl
  (:use :common-lisp :sb-tcl/conditions :sb-tcl/interface)
  (:export
   ;; sb-tcl/conditions
   :tcl-condition :tcl-error :tcl-result-error :tcl-conversion-error
   :tcl-command-error :tcl-command-return :tcl-command-break :tcl-command-continue
   :tcl-interpreter-error
   ;; sb-tcl/interface
   :*libtcl-name* :*libtcl*
   :*libtk-name* :*libtk*
   :open-libtcl :close-libtcl
   :open-libtk :close-libtk
   :read-tcl-script :*tcl-preamble*
   :tcl-interpreter :*tcl-interpreter*
   :start-tcl-interpreter :stop-tcl-interpreter
   :initialize-tcl :initialize-tk :enter-tk-main-loop
   :to-tcl :from-tcl-as
   :interpret-tcl :get-tcl-result-as
   :tcl-command-call :define-tcl-callout
   :define-tcl-command :register-tcl-command))
