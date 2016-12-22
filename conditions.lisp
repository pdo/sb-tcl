;;;
;;; Copyright (c) 2016 Paul Onions
;;; Licence: MIT, see LICENCE file for details
;;;
;;; Conditions signaled by SB-TCL.
;;;
(in-package :sb-tcl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Base conditions

(define-condition tcl-condition ()
  ((msg  :initform "<no message>" :initarg :msg  :reader message)
   (args :initform nil            :initarg :args :reader arguments)))

(defmethod print-object ((condition tcl-condition) strm)
  (format strm (message condition) (arguments condition)))

(define-condition tcl-error (tcl-condition)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Errors when retrieving results from a Tcl interpreter

(define-condition tcl-result-error (tcl-error)
  ())

(defmethod print-object ((condition tcl-result-error) strm)
  (format strm "Tcl Result Error: ~A" (message condition)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Errors when converting between Tcl and Lisp objects

(define-condition tcl-conversion-error (tcl-error)
  ())

(defmethod print-object ((condition tcl-conversion-error) strm)
  (format strm "Tcl Conversion Error: ~A" (message condition)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tcl command return conditions

(define-condition tcl-command-error (tcl-error)
  ())

(defmethod print-object ((condition tcl-command-error) strm)
  (format strm "Tcl Command Error: ~A" (message condition)))

(define-condition tcl-command-return (tcl-condition)
  ())

(define-condition tcl-command-break (tcl-condition)
  ())

(define-condition tcl-command-continue (tcl-condition)
  ())
