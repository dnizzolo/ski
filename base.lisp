(in-package #:ski)

(defclass term () ()
  (:documentation "The base class for any kind of term."))

(defun term-p (object)
  "Return true if OBJECT is a TERM, and NIL otherwise."
  (typep object 'term))

(defgeneric term-equal (term1 term2)
  (:documentation "Return true if the two terms are exactly equal, and NIL otherwise."))

(defgeneric print-term (term &optional stream)
  (:documentation "Print a parsable representation of the TERM to the specified STREAM."))

(defclass variable ()
  ((name :initarg :name :accessor variable-name))
  (:documentation "The base class for a variable."))

(defun variable-p (object)
  "Return true if OBJECT is a VARIABLE, and NIL otherwise."
  (typep object 'variable))

(defmethod print-object ((object variable) stream)
  (with-slots (name) object
    (print-unreadable-object (object stream :type t :identity t)
      (format stream "(~A)" name))))

(defgeneric same-variable-p (variable1 variable2)
  (:documentation "Return true if the two variable are the equivalent, and NIL otherwise."))

(defmethod same-variable-p ((variable1 variable) (variable2 variable))
  (eql (variable-name variable1) (variable-name variable2)))

(defclass application ()
  ((left :initarg :left :accessor application-left)
   (right :initarg :right :accessor application-right))
  (:documentation "The base class for an application."))

(defun application-p (object)
  "Return true if OBJECT is an APPLICATION, and NIL otherwise."
  (typep object 'application))

(defmethod print-object ((object application) stream)
  (with-slots (left right) object
    (print-unreadable-object (object stream :type t :identity t)
      (format stream "(~A ~A)" left right))))

(defgeneric occurs-free-p (variable term)
  (:documentation "Return true if VARIABLE is free in TERM, and NIL otherwise."))

(defmethod occurs-free-p ((variable variable) (term variable))
  (same-variable-p variable term))

(defmethod occurs-free-p ((variable variable) (term application))
  (with-accessors ((left application-left)
                   (right application-right))
      term
    (or (occurs-free-p variable left) (occurs-free-p variable right))))

(defgeneric reduce-term (term)
  (:documentation "Return the normal form of TERM."))
