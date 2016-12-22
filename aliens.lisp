;;;
;;; Copyright (c) 2016 Paul Onions
;;; Licence: MIT, see LICENCE file for details
;;;
;;; SBCL foreign definitions
;;;
(in-package :sb-tcl-alien)

;;; Opaque foreign structures
(define-alien-type interp-ptr  (* (struct nil)))
(define-alien-type obj-ptr     (* (struct nil)))
(define-alien-type command-ptr (* (struct nil)))
(define-alien-type data-ptr    (* (struct nil)))

;;; Tcl library routines
(define-alien-routine ("Tcl_FindExecutable" tcl-find-executable) void
  (argv0 c-string))

(define-alien-routine ("Tcl_CreateInterp" tcl-create-interp) interp-ptr)
    
(define-alien-routine ("Tcl_DeleteInterp" tcl-delete-interp) void
  (interp interp-ptr))

(define-alien-routine ("Tcl_Init" tcl-init) int
  (interp interp-ptr))

(define-alien-routine ("Tcl_EvalEx" tcl-eval-ex) int
  (interp interp-ptr) (script c-string) (num-bytes int) (flags int))

(define-alien-routine ("Tcl_EvalObjEx" tcl-eval-obj-ex) int
  (interp interp-ptr) (obj obj-ptr) (flags int))

(define-alien-routine ("Tcl_GetStringResult" tcl-get-string-result) c-string
  (interp interp-ptr))

(define-alien-routine ("Tcl_GetObjResult" tcl-get-obj-result) obj-ptr
  (interp interp-ptr))

(define-alien-routine ("Tcl_NewObj" tcl-new-obj) obj-ptr)

(define-alien-routine ("Tcl_NewStringObj" tcl-new-string-obj) obj-ptr
  (bytes c-string) (length int))

(define-alien-routine ("Tcl_NewLongObj" tcl-new-long-obj) obj-ptr
  (long-value long))

(define-alien-routine ("Tcl_NewDoubleObj" tcl-new-double-obj) obj-ptr
  (double-value double))

(define-alien-routine ("Tcl_ListObjAppendElement" tcl-list-obj-append-element) int
  (interp interp-ptr) (list obj-ptr) (obj obj-ptr))

(define-alien-routine ("Tcl_GetStringFromObj" tcl-get-string-from-obj) c-string
  (obj obj-ptr) (length-ptr int :out))

(define-alien-routine ("Tcl_GetLongFromObj" tcl-get-long-from-obj) int
  (interp interp-ptr) (obj obj-ptr) (long-ptr long :out))

(define-alien-routine ("Tcl_GetDoubleFromObj" tcl-get-double-from-obj) int
  (interp interp-ptr) (obj obj-ptr) (double-ptr double :out))

(define-alien-routine ("Tcl_DbIncrRefCount" tcl-db-incr-ref-count) void
  (obj obj-ptr) (--file-- c-string) (--line-- int))

(define-alien-routine ("Tcl_DbDecrRefCount" tcl-db-decr-ref-count) void
  (obj obj-ptr) (--file-- c-string) (--line-- int))

(define-alien-routine ("Tcl_CreateObjCommand" tcl-create-obj-command) command-ptr
  (interp interp-ptr)
  (cmd-name c-string)
  (proc (function int data-ptr interp-ptr int (* (array obj-ptr nil))))
  (client-data data-ptr)
  (delete-proc (function void data-ptr)))

(define-alien-routine ("Tcl_WrongNumArgs" tcl-wrong-num-args) void
  (interp interp-ptr) (objc int) (objv (* (array obj-ptr nil))) (message c-string))

(define-alien-routine ("Tcl_SetObjResult" tcl-set-obj-result) void
  (interp interp-ptr) (obj obj-ptr))

;;; Tk library routines
(define-alien-routine ("Tk_Init" tk-init) int
  (interp interp-ptr))

(define-alien-routine ("Tk_MainLoop" tk-main-loop) void)
