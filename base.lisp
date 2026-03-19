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
  ((name :initarg :name :accessor name))
  (:default-initargs
   :name "Name required.")
  (:documentation "The base class for a variable."))

(defmethod initialize-instance :after ((variable variable) &key)
  (with-slots (name) variable
    (setf name (etypecase name
                 (string name)
                 (character (string name))
                 (symbol (string-downcase (symbol-name name)))))))

(defun variable-p (object)
  "Return true if OBJECT is a VARIABLE, and NIL otherwise."
  (typep object 'variable))

(defmethod print-object ((object variable) stream)
  (with-slots (name) object
    (print-unreadable-object (object stream :type t :identity t)
      (princ name stream))))

(defmethod print-term ((term variable) &optional (stream *standard-output*))
  (with-slots (name) term
    (princ name stream))
  term)

(defun variable->symbol (variable)
  "Return a symbol that has the same name as VARIABLE."
  (intern (string-upcase (name variable))))

(defclass variable-name-generator ()
  ((state :initform #\a))
  (:documentation "Generator object to automatically create variable names."))

(defun make-variable-name-generator ()
  "Construct and return a VARIABLE-NAME-GENERATOR."
  (make-instance 'variable-name-generator))

(defun generate-name (generator)
  "Return the next variable name from GENERATOR."
  (with-slots (state) generator
    (if (integerp state)
        (prog1 (format nil "_~d" state) (incf state))
        (shiftf state (if (char= state #\z) 0 (code-char (1+ (char-code state))))))))

(defgeneric same-variable-p (variable1 variable2)
  (:documentation "Return true if VARIABLE1 and VARIABLE2 have the same name, and NIL
otherwise."))

(defmethod same-variable-p ((variable1 variable) (variable2 variable))
  (string= (name variable1) (name variable2)))

(defclass application ()
  ((left :initarg :left :accessor left)
   (right :initarg :right :accessor right))
  (:documentation "The base class for an application."))

(defun application-p (object)
  "Return true if OBJECT is an APPLICATION, and NIL otherwise."
  (typep object 'application))

(defmethod print-object ((object application) stream)
  (with-slots (left right) object
    (print-unreadable-object (object stream :type t :identity t)
      (format stream "(~a ~a)" left right))))

(defgeneric occurs-free-p (variable term)
  (:documentation "Return true if VARIABLE is free in TERM, and NIL otherwise."))

(defmethod occurs-free-p ((variable variable) (term variable))
  (same-variable-p variable term))

(defmethod occurs-free-p ((variable variable) (term application))
  (with-slots (left right) term
    (or (occurs-free-p variable left) (occurs-free-p variable right))))

(defgeneric reduce-term (term)
  (:documentation "Return the normal form of TERM."))
