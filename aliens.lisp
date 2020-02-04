;;;
;;; Copyright (c) 2016 - 2020 Paul Onions
;;; Licence: MIT, see LICENCE file for details
;;;
;;; SBCL foreign definitions
;;;
(in-package :sb-tcl-alien)

;;; Opaque foreign structures
(define-alien-type tcl-interp-ptr  (* (struct nil)))
(define-alien-type tcl-obj-ptr     (* (struct nil)))
(define-alien-type tcl-command-ptr (* (struct nil)))
(define-alien-type tcl-data-ptr    (* (struct nil)))

;;; Tcl library routines
(define-alien-routine ("Tcl_FindExecutable" tcl-find-executable) void
  (argv0 c-string))

(define-alien-routine ("Tcl_CreateInterp" tcl-create-interp) tcl-interp-ptr)
    
(define-alien-routine ("Tcl_DeleteInterp" tcl-delete-interp) void
  (interp tcl-interp-ptr))

(define-alien-routine ("Tcl_Init" tcl-init) int
  (interp tcl-interp-ptr))

(define-alien-routine ("Tcl_EvalEx" tcl-eval-ex) int
  (interp tcl-interp-ptr) (script c-string) (num-bytes int) (flags int))

(define-alien-routine ("Tcl_EvalObjEx" tcl-eval-obj-ex) int
  (interp tcl-interp-ptr) (obj tcl-obj-ptr) (flags int))

(define-alien-routine ("Tcl_GetStringResult" tcl-get-string-result) c-string
  (interp tcl-interp-ptr))

(define-alien-routine ("Tcl_GetObjResult" tcl-get-obj-result) tcl-obj-ptr
  (interp tcl-interp-ptr))

(define-alien-routine ("Tcl_NewObj" tcl-new-obj) tcl-obj-ptr)

(define-alien-routine ("Tcl_NewStringObj" tcl-new-string-obj) tcl-obj-ptr
  (bytes c-string) (length int))

(define-alien-routine ("Tcl_NewLongObj" tcl-new-long-obj) tcl-obj-ptr
  (long-value long))

(define-alien-routine ("Tcl_NewDoubleObj" tcl-new-double-obj) tcl-obj-ptr
  (double-value double))

(define-alien-routine ("Tcl_NewListObj" tcl-new-list-obj) tcl-obj-ptr
  (objc int) (objv (* (array tcl-obj-ptr nil))))

(define-alien-routine ("Tcl_ListObjAppendElement" tcl-list-obj-append-element) int
  (interp tcl-interp-ptr) (list tcl-obj-ptr) (obj tcl-obj-ptr))

;;(define-alien-routine ("Tcl_ListObjGetElements" tcl-list-obj-get-elements) int
;;  (interp tcl-interp-ptr) (list tcl-obj-ptr) (objc-ptr int :out) (objv-ptr (* (array tcl-obj-ptr nil))))

(define-alien-routine ("Tcl_ListObjIndex" tcl-list-obj-index) int
  (interp tcl-interp-ptr) (list tcl-obj-ptr) (index int) (elem (* tcl-obj-ptr)))

(define-alien-routine ("Tcl_ListObjLength" tcl-list-obj-length) int
  (interp tcl-interp-ptr) (list tcl-obj-ptr) (length-ptr int :out))

(define-alien-routine ("Tcl_GetStringFromObj" tcl-get-string-from-obj) c-string
  (obj tcl-obj-ptr) (length-ptr int :out))

(define-alien-routine ("Tcl_GetLongFromObj" tcl-get-long-from-obj) int
  (interp tcl-interp-ptr) (obj tcl-obj-ptr) (long-ptr long :out))

(define-alien-routine ("Tcl_GetDoubleFromObj" tcl-get-double-from-obj) int
  (interp tcl-interp-ptr) (obj tcl-obj-ptr) (double-ptr double :out))

(define-alien-routine ("Tcl_DbIncrRefCount" tcl-db-incr-ref-count) void
  (obj tcl-obj-ptr) (--file-- c-string) (--line-- int))

(define-alien-routine ("Tcl_DbDecrRefCount" tcl-db-decr-ref-count) void
  (obj tcl-obj-ptr) (--file-- c-string) (--line-- int))

(define-alien-routine ("Tcl_CreateObjCommand" tcl-create-obj-command) tcl-command-ptr
  (interp tcl-interp-ptr)
  (cmd-name c-string)
  (proc (function int tcl-data-ptr tcl-interp-ptr int (* (array tcl-obj-ptr nil))))
  (client-data tcl-data-ptr)
  (delete-proc (function void tcl-data-ptr)))

(define-alien-routine ("Tcl_WrongNumArgs" tcl-wrong-num-args) void
  (interp tcl-interp-ptr) (objc int) (objv (* (array tcl-obj-ptr nil))) (message c-string))

(define-alien-routine ("Tcl_SetObjResult" tcl-set-obj-result) void
  (interp tcl-interp-ptr) (obj tcl-obj-ptr))

;;; Tk library routines
(define-alien-routine ("Tk_Init" tk-init) int
  (interp tcl-interp-ptr))

(define-alien-routine ("Tk_MainLoop" tk-main-loop) void)
