;;;
;;; Copyright (c) 2016 - 2018 Paul Onions
;;; Licence: MIT, see LICENCE file for details
;;;
;;; SBCL<->Tcl interface.
;;;
(in-package :sb-tcl)

(defparameter +fixnum-is-tcl-long+ t)  ; !!! TODO auto detect !!!

(defparameter +tcl-long-max+ (- (expt 2 31) 1))  ; !!! TODO auto detect !!!

(defparameter +tcl-long-min+ (- (expt 2 31)))  ; !!! TODO auto detect !!!

;;; Interpreter return codes
(defparameter +tcl-ok+       0)
(defparameter +tcl-error+    1)
(defparameter +tcl-return+   2)
(defparameter +tcl-break+    3)
(defparameter +tcl-continue+ 4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tcl library

(defvar *libtcl-location* "/usr/lib64/libtcl8.6.so")

(defvar *libtcl* nil)

(defun open-libtcl (&optional pathname)
  (unless *libtcl*
    (setf *libtcl* (load-shared-object (or pathname *libtcl-location*)))
    (tcl-find-executable (first sb-ext:*posix-argv*)))
  *libtcl*)

(defun close-libtcl (&optional pathname)
  (unload-shared-object (or pathname *libtcl-location*))
  (setf *libtcl* nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tk library

(defvar *libtk-location* "/usr/lib64/libtk8.6.so")

(defvar *libtk* nil)

(defun open-libtk (&optional pathname)
  (unless *libtk*
    (setf *libtk* (load-shared-object (or pathname *libtk-location*)))))

(defun close-libtk (&optional pathname)
  (unload-shared-object (or pathname *libtk-location*))
  (setf *libtk* nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tcl interpreter

(defvar *tcl-interpreter* nil
  "The default current Tcl interpreter.")

(defun start-tcl-interpreter (&key with-tk no-bind)
  "Return a newly created Tcl interpreter.

Also bind it to *TCL-INTERPRETER* if NO-BIND is NIL."
  (unless *libtcl*
    (open-libtcl))
  (let ((interp (tcl-create-interp)))
    (initialize-tcl interp)
    (when with-tk
      (unless *libtk*
        (open-libtk))
      (initialize-tk interp))
    (unless no-bind
      (setf *tcl-interpreter* interp))
    interp))

(defun initialize-tcl (&optional (interp *tcl-interpreter*))
  (unless (zerop (tcl-init interp))
    (error 'tcl-error
           :msg "Error during Tcl initialization: ~A"
           :args (tcl-get-string-result interp))))

(defun initialize-tk (&optional (interp *tcl-interpreter*))
  (unless (zerop (tk-init interp))
    (error 'tcl-error
           :msg "Error during Tk initialization: ~A"
           :args (tcl-get-string-result *tcl-interpreter*)))
  *tcl-interpreter*)

(defun enter-tk-main-loop ()
  (tk-main-loop))

(defun stop-tcl-interpreter (&optional (interp *tcl-interpreter*))
  "Destroy the Tcl interpreter."
  (tcl-delete-interp interp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lisp->Tcl conversions

(defgeneric to-tcl (obj)
  (:documentation "Convert OBJ to a Tcl foreign object."))

(defmethod to-tcl ((obj string))
  "Return a newly constructed Tcl string foreign object."
  (tcl-new-string-obj obj -1))

(defmethod to-tcl ((obj fixnum))
  "Return a newly constructed Tcl long-integer foreign object, if possible."
  (if (or +fixnum-is-tcl-long+ (<= +tcl-long-min+ obj +tcl-long-max+))
      (tcl-new-long-obj obj)
      (error 'tcl-conversion-error
             :msg "TO-TCL Error: ~A is too big for a Tcl long object."
             :args obj)))

(defmethod to-tcl ((obj integer))
  "Return a newly constructed Tcl long-integer foreign object, if possible."
  (if (<= +tcl-long-min+ obj +tcl-long-max+)
      (tcl-new-long-obj obj)
      (error 'tcl-conversion-error
             :msg "TO-TCL Error: ~A is too big for a Tcl long object."
             :args obj)))

(defmethod to-tcl ((obj single-float))
  "Return a newly constructed Tcl double-float foreign object"
  (tcl-new-double-obj (coerce obj 'double-float)))

(defmethod to-tcl ((obj double-float))
  "Return a newly constructed Tcl double-float foreign object"
  (tcl-new-double-obj obj))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tcl->Lisp conversions

(defgeneric from-tcl-as (type tcl-obj)
  (:documentation "Convert TCL-OBJ to a normal Lisp object."))

(defmethod from-tcl-as ((type (eql 'string)) tcl-obj)
  "Get the string representation of a Tcl object.

Return a Lisp string."
  (tcl-get-string-from-obj tcl-obj))

(defmethod from-tcl-as ((type (eql 'integer)) tcl-obj)
  "Get the integer representation of a Tcl object.

Return a Lisp integer.  Currently, if the Tcl result cannot be
obtained in long-integer form then signal an error of type
TCL-CONVERSION-ERROR."
  (multiple-value-bind (stat val) (tcl-get-long-from-obj *tcl-interpreter* tcl-obj)
    (unless (eql stat +tcl-ok+)
      (error 'tcl-conversion-error 
             :msg (get-tcl-result-as 'string)))
    val))

(defmethod from-tcl-as ((type (eql 'double-float)) tcl-obj)
  "Get the double-float representation of a Tcl object.

Return a Lisp double-float.  If the Tcl result cannot be obtained in
double-float form then signal an error of type TCL-CONVERSION-ERROR."
  (multiple-value-bind (stat val) (tcl-get-double-from-obj *tcl-interpreter* tcl-obj)
    (unless (eql stat +tcl-ok+)
      (error 'tcl-conversion-error 
             :msg (get-tcl-result-as 'string)))
    val))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interpreter invocation

(defgeneric interpret-tcl (object &key &allow-other-keys)
  (:documentation
   "Evaluate the given Lisp OBJECT using the Tcl interpreter."))

(defmethod interpret-tcl ((script string) &key)
  (tcl-eval-ex *tcl-interpreter* script -1 0))

(defgeneric get-tcl-result-as (type &key &allow-other-keys)
  (:documentation "Get the Tcl interpreter result, of the specified type, if possible."))

(defmethod get-tcl-result-as ((type (eql 'string)) &key)
  "Get the string representation of the Tcl interpreter result.

Return a Lisp string."
  (tcl-get-string-result *tcl-interpreter*))

(defmethod get-tcl-result-as ((type (eql 'integer)) &key)
  "Get the integer representation of the Tcl interpreter result.

Return a Lisp integer.  Currently, if the Tcl result cannot be
obtained in long-integer form then signal an error of type
TCL-RESULT-ERROR."
  (let ((foreign-obj (tcl-get-obj-result *tcl-interpreter*)))
    (multiple-value-bind (stat val) (tcl-get-long-from-obj *tcl-interpreter* foreign-obj)
      (unless (eql stat +tcl-ok+)
        (error 'tcl-result-error 
               :msg (get-tcl-result-as 'string)))
      val)))

(defmethod get-tcl-result-as ((type (eql 'double-float)) &key)
  "Get the double-float representation of the Tcl interpreter result.

Return a Lisp double-float.  If the Tcl result cannot be obtained in
double-float form then signal an error of type TCL-RESULT-ERROR."
  (let ((foreign-obj (tcl-get-obj-result *tcl-interpreter*)))
    (multiple-value-bind (stat val) (tcl-get-double-from-obj *tcl-interpreter* foreign-obj)
      (unless (eql stat +tcl-ok+)
        (error 'tcl-result-error 
               :msg (get-tcl-result-as 'string)))
      val)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Commands

(defun tcl-command-call (&rest objs)
  "Construct and call a Tcl command.

Collect the results of passing each object from OBJS through TO-TCL to
form a Tcl list object.  Then invoke the interpreter on it, returning
a status code."
  (let ((foreign-obj (tcl-new-obj)))
    (loop
       :for obj :in objs
       :do (tcl-list-obj-append-element *tcl-interpreter* foreign-obj (to-tcl obj)))
    (tcl-eval-obj-ex *tcl-interpreter* foreign-obj 0)))

(defmacro define-tcl-callout (name lambda-list result-type &body body)
  "Define a Lisp function that calls a Tcl command.

Construct a Tcl command line and invoke it.

The Tcl command line is constructed in Lisp by wrapping BODY in a
function definition based on NAME and LAMBDA-LIST, the last form in
the body is expected to evaluate to a list of objects which will be
given to TCL-COMMAND-CALL to make the call into Tcl.

The result of the Tcl evaluation will be returned as type RESULT-TYPE
if possible, otherwise TCL-RESULT-ERROR condition will be signalled.

May also signal a TCL-COMMAND-ERROR, TCL-COMMAND-RETURN,
TCL-COMMAND-BREAK or TCL-COMMAND-CONTINUE condition if the command
does not complete with a +TCL-OK+ return code."
  (let ((!args (gensym "ARGS"))
        (!stat (gensym "STAT")))
    `(defun ,name ,lambda-list
       (let* ((,!args (progn ,@body))
              (,!stat (apply #'tcl-command-call ,!args)))
         (ecase ,!stat
           ((,+tcl-ok+)       ,(and result-type `(get-tcl-result-as ',result-type)))
           ((,+tcl-error+)    (error  'tcl-command-error :msg (get-tcl-result-as 'string)))
           ((,+tcl-return+)   (signal 'tcl-command-return))
           ((,+tcl-break+)    (signal 'tcl-command-break))
           ((,+tcl-continue+) (signal 'tcl-command-continue)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Callbacks

(defun %wrong-num-args (interp objv cmsg)
  (tcl-wrong-num-args interp 1 objv cmsg))

(defun %set-obj-result (interp rslt)
  (tcl-set-obj-result interp rslt))

(defmacro define-tcl-callback (name lambda-list &body body)
  "Define a Lisp function callable from Tcl.

Wrap BODY in a function based on NAME and LAMBDA-LIST, where
LAMBDA-LIST is a list of (NAME TYPE) pairs, so that BODY is evaluated
in a context where each NAME is bound to the result of applying
FROM-TCL-AS to TYPE and the tcl object supplied by Tcl.

The primary result of evaluating BODY will be returned to Tcl via the
TO-TCL generic function, except if it is NIL, in which case it is
assumed that the command return value has been set by other means
inside BODY and is left untouched.

An optional second result should, if supplied, be an interpreter exit
status code.  If it is omitted, or nil, then +TCL-OK+ is assumed."
  (let ((arity (length lambda-list))
        (args-descrip (format nil "~{~{<~A/~A>~}~^ ~}" lambda-list))
        (!client-data (gensym "CDAT"))
        (!interp (gensym "TCLI"))
        (!objc (gensym "OBJC"))
        (!objv (gensym "OBJV"))
        (!rslt (gensym "RSLT"))
        (!stat (gensym "STAT")))
    `(define-alien-callback ,name int
       ((,!client-data data-ptr)
        (,!interp interp-ptr)
        (,!objc int)
        (,!objv (* (array obj-ptr nil))))
       ;;(declare (ignore ,!client-data))
       (multiple-value-bind (,!rslt ,!stat)
           (cond
             ((/= ,!objc ,(1+ arity))
              (%wrong-num-args ,!interp ,!objv ,args-descrip)
              (values nil +tcl-error+))
             (t
              (let (,@(loop
                         :for (name type) :in lambda-list
                         :for idx :from 1
                         :collect `(,name (from-tcl-as ',type (deref (deref ,!objv) ,idx)))))
                ,@body)))
         (when ,!rslt
           (%set-obj-result *tcl-interpreter* (to-tcl ,!rslt)))
         (or ,!stat +tcl-ok+)))))

(define-alien-callback tcl-delete-command void ((client-data data-ptr))
  ;;(declare (ignore client-data))
  nil)

(defun register-tcl-callback (callback &optional name)
  "Register CALLBACK with *TCL-INTERPRETER*.

The CALLBACK argument should be a symbol that has previously been
bound to a callback function by a DEFINE-TCL-CALLBACK form.  The
callback function will become accessible in the Tcl interpreter as
NAME, defaulting to a downcased version of the Lisp symbol name if
omitted."
  (let ((str (or name (string-downcase (string callback)))))
    (tcl-create-obj-command *tcl-interpreter*
                            str
                            (symbol-value callback)
                            nil
                            tcl-delete-command)))
