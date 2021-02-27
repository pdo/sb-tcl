;;;
;;; Copyright (c) 2016 - 2020 Paul Onions
;;; Licence: MIT, see LICENCE file for details
;;;
;;; SBCL<->Tcl interface.
;;;
(in-package :sb-tcl)

;;; Assume SBCL and Tcl are either both 64-bit or both 32-bit.
(defparameter +fixnum-is-tcl-long+
  #+(AND OS-WINDOWS 64-BIT) nil
  #-(AND OS-WINDOWS 64-BIT) t)

(defparameter +tcl-long-max+
  #+64-BIT (- (expt 2 63) 1)
  #-64-BIT (- (expt 2 31) 1))

(defparameter +tcl-long-min+
  #+64-BIT (- (expt 2 63))
  #-64-BIT (- (expt 2 31)))

;;; Interpreter return codes
(defparameter +tcl-ok+       0)
(defparameter +tcl-error+    1)
(defparameter +tcl-return+   2)
(defparameter +tcl-break+    3)
(defparameter +tcl-continue+ 4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tcl library

(defvar *libtcl-name*
  #+LINUX      '("libtcl8.6.so" "libtcl.so")
  #+FREEBSD    '("libtcl86.so" "libtcl.so")
  #+DARWIN     '("libtcl8.6.dylib" "Tcl.framework/Versions/8.6/Tcl" "/opt/local/lib/libtcl8.6.dylib")
  #+OS-WINDOWS '("tcl86.dll" "tcl.dll")
  "Name of the libtcl library, or a list of candidate names to be
  tried in order.")

(defvar *libtcl* nil)

(defun open-libtcl ()
  (unless *libtcl*
    (let ((names (if (listp *libtcl-name*) *libtcl-name* (list *libtcl-name*))))
      (loop
         :for name :in names
         :do (setf *libtcl* (ignore-errors (load-shared-object name :dont-save t)))
         :until *libtcl*)))
  (if *libtcl*
      (tcl-find-executable (first sb-ext:*posix-argv*))
      (error "Unable to load the libtcl library.")))

(defun close-libtcl ()
  (unload-shared-object *libtcl*)
  (setf *libtcl* nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tk library

(defvar *libtk-name*
  #+LINUX      '("libtk8.6.so" "libtk.so")
  #+FREEBSD    '("libtk86.so" "libtk.so")
  #+DARWIN     '("libtk8.6.dylib" "Tk.framework/Versions/8.6/Tk" "/opt/local/lib/libtk8.6.dylib")
  #+OS-WINDOWS '("tk86.dll" "tk.dll")
  "Name of the libtk library, or a list of candidate names to be
  tried in order.")

(defvar *libtk* nil)

(defun open-libtk ()
  (unless *libtk*
    (let ((names (if (listp *libtk-name*) *libtk-name* (list *libtk-name*))))
      (loop
         :for name :in names
         :do (setf *libtk* (ignore-errors (load-shared-object name :dont-save t)))
         :until *libtk*)))
  (unless *libtk*
    (error "Unable to load the libtk library.")))

(defun close-libtk ()
  (unload-shared-object *libtk*)
  (setf *libtk* nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tcl interpreter

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun read-tcl-script (filespec)
    (with-open-file (strm filespec)
      (let* ((buffer (make-array (file-length strm)
                                 :element-type (stream-element-type strm)
                                 :fill-pointer t))
             (position (read-sequence buffer strm)))
        (setf (fill-pointer buffer) position)
        buffer))))

(defvar *tcl-preamble*
  #.(read-tcl-script (merge-pathnames "tcl-preamble.tcl" *compile-file-truename*))
  "Tcl script to execute when interpreter is started.")

(defvar *tcl-interpreter* nil
  "The default current Tcl interpreter.")

(defun start-tcl-interpreter (&key with-tk no-bind no-preamble)
  "Return a newly created Tcl interpreter.
Also bind it to *TCL-INTERPRETER* if NO-BIND is NIL, and source
tcl-preamble.tcl if NO-PREAMBLE is NIL."
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
    (unless no-preamble
      (interpret-tcl *tcl-preamble*))
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
;;; Lisp->Tcl conversions

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

(defmethod to-tcl ((obj list))
  "Return a newly constructed Tcl list foreign object"
  (let ((tcl-list (tcl-new-list-obj 0 nil)))
    (loop
       :for elem :in obj
       :do (tcl-list-obj-append-element
            *tcl-interpreter* tcl-list (to-tcl elem)))
    tcl-list))

(defmethod to-tcl ((obj vector))
  "Return a newly constructed Tcl list foreign object"
  (let ((tcl-list (tcl-new-list-obj 0 nil)))
    (loop
       :for elem :across obj
       :do (tcl-list-obj-append-element
            *tcl-interpreter* tcl-list (to-tcl elem)))
    tcl-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tcl->Lisp conversions

(defgeneric from-tcl-as (type tcl-obj &rest parameters)
  (:documentation "Convert TCL-OBJ to a normal Lisp object of type TYPE."))

(defmethod from-tcl-as ((type (eql 'string)) tcl-obj &rest parameters)
  "Get the string representation of a Tcl object."
  (declare (ignore parameters))
  (tcl-get-string-from-obj tcl-obj))

(defmethod from-tcl-as ((type (eql 'integer)) tcl-obj &rest parameters)
  "Get the integer representation of a Tcl object.

Return a Lisp integer.  Currently, if the Tcl result cannot be
obtained in long-integer form then signal an error of type
TCL-CONVERSION-ERROR."
  (declare (ignore parameters))
  (multiple-value-bind (stat val) (tcl-get-long-from-obj *tcl-interpreter* tcl-obj)
    (unless (eql stat +tcl-ok+)
      (error 'tcl-conversion-error 
             :msg (get-tcl-result-as 'string)))
    val))

(defmethod from-tcl-as ((type (eql 'double-float)) tcl-obj &rest parameters)
  "Get the double-float representation of a Tcl object.

Return a Lisp double-float.  If the Tcl result cannot be obtained in
double-float form then signal an error of type TCL-CONVERSION-ERROR."
  (declare (ignore parameters))
  (multiple-value-bind (stat val) (tcl-get-double-from-obj *tcl-interpreter* tcl-obj)
    (unless (eql stat +tcl-ok+)
      (error 'tcl-conversion-error 
             :msg (get-tcl-result-as 'string)))
    val))

(defmethod from-tcl-as ((type list) tcl-obj &rest parameters)
  "Get the given representation of a Tcl object.

Where TYPE is a list-based type-desciptor."
  (declare (ignore parameters))
  (let ((constructor (first type))
        (type-params (rest type)))
    (funcall #'from-tcl-as constructor tcl-obj type-params)))

(defmethod from-tcl-as ((constructor (eql 'list)) tcl-obj &rest parameters)
  "Get the given representation of a Tcl object.

Return a list of things, each of a type that is described by the first
element of PARAMETERS (a type descriptor)."
  (multiple-value-bind (stat len)
      (tcl-list-obj-length *tcl-interpreter* tcl-obj)
    (unless (eql stat +tcl-ok+)
      (error 'tcl-conversion-error 
             :msg (get-tcl-result-as 'string)))
    (loop
       :for n :from 0 :below len
       :collect (with-alien ((elem tcl-obj-ptr))
                  (let ((stat (tcl-list-obj-index *tcl-interpreter*
                                                  tcl-obj n (addr elem))))
                    (unless (eql stat +tcl-ok+)
                      (error 'tcl-conversion-error 
                             :msg (get-tcl-result-as 'string)))
                    (from-tcl-as (first parameters) elem))))))

(defmethod from-tcl-as ((constructor (eql 'vector)) tcl-obj &rest parameters)
  "Get the given representation of a Tcl object.

Return a vector of things, each of a type that is described by the
first element of PARAMETERS (a type descriptor)."
  (multiple-value-bind (stat len)
      (tcl-list-obj-length *tcl-interpreter* tcl-obj)
    (unless (eql stat +tcl-ok+)
      (error 'tcl-conversion-error 
             :msg (get-tcl-result-as 'string)))
    (let ((vec (make-array len :initial-element nil)))
      (loop
         :for n :from 0 :below len
         :do 
           (with-alien ((elem (* tcl-obj-ptr)))
             (let ((stat (tcl-list-obj-index *tcl-interpreter*
                                             tcl-obj n elem)))
               (unless (eql stat +tcl-ok+)
                 (error 'tcl-conversion-error 
                        :msg (get-tcl-result-as 'string)))
               (setf (aref vec n)
                     (from-tcl-as (first parameters) elem)))))
      vec)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Interpreter invocation

(defgeneric interpret-tcl (object &key &allow-other-keys)
  (:documentation
   "Evaluate the given Lisp OBJECT using the Tcl interpreter."))

(defmethod interpret-tcl ((script string) &key)
  (tcl-eval-ex *tcl-interpreter* script -1 0))

(defun get-tcl-result-as (type)
  "Get the desired representation of the Tcl interpreter result.

If the Tcl result cannot be obtained in TYPE form then signal an error
of type TCL-RESULT-ERROR."
  (let ((tcl-obj (tcl-get-obj-result *tcl-interpreter*)))
    (handler-case
        (from-tcl-as type tcl-obj)
      (tcl-conversion-error (condition)
        (error 'tcl-result-error
               :msg (message condition))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tcl Callouts

(defun tcl-command-call (&rest objs)
  "Construct a Tcl command line and invoke it.

Collect the results of passing each object from OBJS through TO-TCL to
form a Tcl list object.  Then invoke the interpreter on it, returning
a status code."
  (let ((foreign-obj (tcl-new-obj)))
    (loop
       :for obj :in objs
       :do (tcl-list-obj-append-element *tcl-interpreter* foreign-obj (to-tcl obj)))
    (tcl-eval-obj-ex *tcl-interpreter* foreign-obj 0)))

(defmacro define-tcl-callout (name lambda-list result-type &body body)
  "Define a Lisp function that constructs a Tcl command line and invokes it.

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
;;; Tcl Commands

(defun %wrong-num-args (interp objv cmsg)
  (tcl-wrong-num-args interp 1 objv cmsg))

(defun %set-obj-result (interp rslt)
  (tcl-set-obj-result interp rslt))

(defmacro define-tcl-command (name lambda-list &body body)
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
       ((,!client-data tcl-data-ptr)
        (,!interp tcl-interp-ptr)
        (,!objc int)
        (,!objv (* (array tcl-obj-ptr nil))))
       (declare (ignore ,!client-data))
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

(define-alien-callback tcl-delete-command void ((client-data tcl-data-ptr))
  (declare (ignore client-data))
  nil)

(defun register-tcl-command (command &optional name)
  "Register COMMAND with *TCL-INTERPRETER*.

The COMMAND argument should be a symbol that has previously been bound
to a command callback function by a DEFINE-TCL-COMMAND form.  The
callback function will become accessible in the Tcl interpreter as a
command called NAME, defaulting to a downcased version of the Lisp
symbol name if omitted."
  (let ((str (or name (string-downcase (string command)))))
    (tcl-create-obj-command *tcl-interpreter*
                            str
                            (symbol-value command)
                            nil
                            tcl-delete-command)))
