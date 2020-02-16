;;;
;;; Copyright (c) 2016 - 2020 Paul Onions
;;; Licence: MIT, see LICENCE file for details
;;;
;;; Package definition for the SB-TCL system.
;;;
(in-package :cl-user)

(defpackage :sb-tcl-alien
  (:use :common-lisp)
  (:import-from :sb-alien
   #:define-alien-type #:define-alien-routine
   #:struct #:void #:int #:long #:double #:c-string)
  (:export
   ;; aliens.lisp
   #:tcl-interp-ptr #:tcl-obj-ptr
   #:tcl-command-ptr #:tcl-data-ptr
   #:tcl-find-executable
   #:tcl-create-interp #:tcl-delete-interp
   #:tcl-init #:tcl-eval-ex #:tcl-eval-obj-ex
   #:tcl-get-string-result #:tcl-get-obj-result
   #:tcl-new-obj #:tcl-new-string-obj
   #:tcl-new-long-obj #:tcl-new-double-obj
   #:tcl-new-list-obj
   #:tcl-list-obj-append-element
   #:tcl-list-obj-index #:tcl-list-obj-length
   #:tcl-get-string-from-obj #:tcl-get-long-from-obj
   #:tcl-get-double-from-obj
   #:tcl-db-incr-ref-count #:tcl-db-decr-ref-count
   #:tcl-create-obj-command #:tcl-wrong-num-args
   #:tcl-set-obj-result
   #:tk-init #:tk-main-loop))

(defpackage :sb-tcl
  (:use :common-lisp :sb-tcl-alien)
  (:import-from :sb-alien
   #:load-shared-object #:unload-shared-object
   #:with-alien #:define-alien-callback
   #:int #:void #:deref #:addr)
  (:export
   ;; conditions.lisp
   #:tcl-condition #:tcl-error #:tcl-result-error #:tcl-conversion-error
   #:tcl-command-error #:tcl-command-return
   #:tcl-command-break #:tcl-command-continue
   ;; interface.lisp
   #:*libtcl-name* #:*libtcl*
   #:*libtk-name* #:*libtk*
   #:open-libtcl #:close-libtcl
   #:open-libtk #:close-libtk
   #:read-tcl-script #:*tcl-preamble*
   #:tcl-interpreter #:*tcl-interpreter*
   #:start-tcl-interpreter #:stop-tcl-interpreter
   #:initialize-tcl #:initialize-tk #:enter-tk-main-loop
   #:to-tcl #:from-tcl-as
   #:interpret-tcl #:get-tcl-result-as
   #:tcl-command-call #:define-tcl-callout
   #:define-tcl-command #:register-tcl-command))
