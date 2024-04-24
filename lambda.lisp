(in-package #:ski)

(defclass lambda-term (term) ()
  (:documentation "The base class for a lambda calculus term."))

(defun lambda-term-p (object)
  "Return true if OBJECT is a LAMBDA-TERM, and NIL otherwise."
  (typep object 'lambda-term))

(defclass lambda-abstraction (lambda-term)
  ((variable :initarg :variable :accessor lambda-abstraction-variable)
   (body :initarg :body :accessor lambda-abstraction-body))
  (:documentation "A lambda abstraction in lambda calculus."))

(defun make-lambda-abstraction (variable body)
  "Construct and return a LAMBDA-ABSTRACTION whose parameter is VARIABLE
and whose body is BODY."
  (make-instance 'lambda-abstraction :variable variable :body body))

(defun lambda-abstraction-p (object)
  "Return true if OBJECT is a LAMBDA-ABSTRACTION, and NIL otherwise."
  (typep object 'lambda-abstraction))

(defmethod print-object ((object lambda-abstraction) stream)
  (with-slots (variable body) object
    (print-unreadable-object (object stream :type t :identity t)
      (format stream "(~A ~A)" variable body))))

(defclass lambda-application (application lambda-term) ()
  (:documentation "An application in lambda calculus."))

(defun make-lambda-application (left right)
  "Construct and return a LAMBDA-APPLICATION that represents the
application of the LEFT term to the RIGHT term."
  (make-instance 'lambda-application :left left :right right))

(defun lambda-application-p (object)
  "Return true if OBJECT is a LAMBDA-APPLICATION, and NIL otherwise."
  (typep object 'lambda-application))

(defclass lambda-variable (variable lambda-term) ()
  (:documentation "A variable in the lambda calculus."))

(defun make-lambda-variable (name)
  "Construct and return a LAMBDA-VARIABLE called NAME."
  (make-instance 'lambda-variable :name name))

(defun lambda-variable-p (object)
  "Return true if OBJECT is a LAMBDA-VARIABLE, and NIL otherwise."
  (typep object 'lambda-variable))

(defmethod term-equal ((term1 lambda-variable)
                       (term2 lambda-variable))
  (same-variable-p term1 term2))

(defmethod term-equal ((term1 lambda-abstraction)
                       (term2 lambda-abstraction))
  (with-accessors ((variable1 lambda-abstraction-variable)
                   (body1 lambda-abstraction-body))
      term1
    (with-accessors ((variable2 lambda-abstraction-variable)
                     (body2 lambda-abstraction-body))
        term2
      (and (term-equal variable1 variable2)
           (term-equal body1 body2)))))

(defmethod term-equal ((term1 lambda-application)
                       (term2 lambda-application))
  (with-accessors ((left1 application-left) (right1 application-right)) term1
    (with-accessors ((left2 application-left) (right2 application-right)) term2
      (and (term-equal left1 left2)
           (term-equal right1 right2)))))

(defmethod print-term ((term lambda-variable)
                       &optional (stream *standard-output*))
  (write-char (variable-name term) stream)
  term)

(defmethod print-term ((term lambda-abstraction)
                       &optional (stream *standard-output*))
  (write-char #\λ stream)
  (print-term (lambda-abstraction-variable term) stream)
  (write-char #\. stream)
  (print-term (lambda-abstraction-body term) stream)
  term)

(defmethod print-term ((term lambda-application)
                       &optional (stream *standard-output*))
  (with-accessors ((left application-left) (right application-right)) term
    (if (lambda-abstraction-p left)
        (progn
          (write-char #\( stream)
          (print-term left stream)
          (write-char #\) stream))
        (print-term left stream))
    (if (lambda-variable-p right)
        (print-term right stream)
        (progn
          (write-char #\( stream)
          (print-term right stream)
          (write-char #\) stream))))
  term)

(defmethod occurs-free-p ((variable variable) (term lambda-abstraction))
  (unless (same-variable-p variable (lambda-abstraction-variable term))
    (occurs-free-p variable (lambda-abstraction-body term))))

(defgeneric free-variables (term)
  (:documentation "Return the free variables in TERM."))

(defmethod free-variables ((term lambda-variable))
  (list term))

(defmethod free-variables ((term lambda-application))
  (with-accessors ((left application-left) (right application-right)) term
    (union (free-variables left)
           (free-variables right)
           :test #'same-variable-p)))

(defmethod free-variables ((term lambda-abstraction))
  (with-accessors ((variable lambda-abstraction-variable)
                   (body lambda-abstraction-body))
      term
    (delete-if (lambda (fv) (same-variable-p fv variable))
               (free-variables body))))

(defgeneric bound-variables (term)
  (:documentation "Return the bound variables in TERM."))

(defmethod bound-variables ((term lambda-variable))
  nil)

(defmethod bound-variables ((term lambda-application))
  (with-accessors ((left application-left) (right application-right)) term
    (union (bound-variables left)
           (bound-variables right)
           :test #'same-variable-p)))

(defmethod bound-variables ((term lambda-abstraction))
  (with-accessors ((variable lambda-abstraction-variable)
                   (body lambda-abstraction-body))
      term
    (union (list variable)
           (bound-variables body)
           :test #'same-variable-p)))

(defgeneric substitute-avoiding-capture (term target replacement)
  (:documentation "Return a new term where the free occurrences of the variable TARGET in
TERM have been replaced with REPLACEMENT without capturing other variables while doing so."))

(defmethod substitute-avoiding-capture ((term lambda-variable)
                                        (target lambda-variable)
                                        (replacement lambda-term))
  (if (same-variable-p term target) replacement term))

(defmethod substitute-avoiding-capture ((term lambda-application)
                                        (target lambda-variable)
                                        (replacement lambda-term))
  (with-accessors ((left application-left) (right application-right)) term
    (make-lambda-application
     (substitute-avoiding-capture left target replacement)
     (substitute-avoiding-capture right target replacement))))

(defmethod substitute-avoiding-capture ((term lambda-abstraction)
                                        (target lambda-variable)
                                        (replacement lambda-term))
  (flet ((fresh-variable (t1 t2)
           (let ((forbidden-names
                   (mapcar #'variable-name
                           (union (free-variables t1)
                                  (free-variables t2)
                                  :test #'same-variable-p))))
             (loop for code from #.(char-code #\a) to #.(char-code #\z)
                   for name = (code-char code)
                   unless (member name forbidden-names)
                     return (make-lambda-variable name)
                   finally (error "Ran out of variable names!")))))
    (with-accessors ((variable lambda-abstraction-variable)
                     (body lambda-abstraction-body))
        term
      (cond ((same-variable-p variable target)
             term)
            ((occurs-free-p variable replacement)
             (let ((fresh-variable (fresh-variable body replacement)))
               (substitute-avoiding-capture
                (make-lambda-abstraction
                 fresh-variable
                 (substitute-avoiding-capture
                  body
                  variable
                  fresh-variable))
                target
                replacement)))
            (t
             (make-lambda-abstraction
              variable
              (substitute-avoiding-capture body target replacement)))))))

(defmethod reduce-term ((term lambda-variable))
  term)

(defmethod reduce-term ((term lambda-abstraction))
  (with-accessors ((variable lambda-abstraction-variable)
                   (body lambda-abstraction-body))
      term
    (make-lambda-abstraction variable (reduce-term body))))

(defmethod reduce-term ((term lambda-application))
  (with-accessors ((left application-left) (right application-right)) term
    (if (lambda-abstraction-p left)
        (reduce-term
         (substitute-avoiding-capture
          (lambda-abstraction-body left)
          (lambda-abstraction-variable left)
          right))
        (let ((reduced-left (reduce-term left)))
          (if (lambda-abstraction-p reduced-left)
              (reduce-term
               (make-lambda-application
                reduced-left
                right))
              (make-lambda-application
               reduced-left
               (reduce-term right)))))))

(defun churchify (n)
  "Convert the natural number N to its representation as a Church
numeral."
  (let ((f (make-lambda-variable #\f))
        (x (make-lambda-variable #\x)))
    (make-lambda-abstraction
     f
     (make-lambda-abstraction
      x
      (let ((acc x))
        (dotimes (i n acc)
          (setf acc (make-lambda-application f acc))))))))

(defun dechurchify (lambda-term)
  "Convert the Church numeral LAMBDA-TERM to its corresponding natural
number."
  (do ((result 0)
       (body (lambda-abstraction-body (lambda-abstraction-body lambda-term))))
      ((lambda-variable-p body) result)
    (incf result)
    (setf body (application-right body))))
