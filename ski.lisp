(in-package #:ski)

(defclass combinator-term (term) ()
  (:documentation "The base class for a combinatory logic term."))

(defun combinator-term-p (object)
  "Return true if OBJECT is a COMBINATOR-TERM, and NIL otherwise."
  (typep object 'combinator-term))

(defclass combinator (combinator-term)
  ((name :initarg :name :accessor combinator-name)
   (arity :initarg :arity :accessor combinator-arity))
  (:documentation "A combinator in combinatory logic."))

(defun make-combinator (name arity)
  "Construct and return a COMBINATOR with name NAME and arity ARITY."
  (make-instance 'combinator :name name :arity arity))

(defun combinator-p (object)
  "Return true if OBJECT is a COMBINATOR, and NIL otherwise."
  (typep object 'combinator))

(defmethod print-object ((object combinator) stream)
  (with-slots (name) object
    (print-unreadable-object (object stream :type t :identity t)
      (format stream "(~A)" name))))

(defclass combinator-application (application combinator-term) ()
  (:documentation "An application for combinatory logic terms."))

(defun make-combinator-application (left right)
  "Construct and return a COMBINATOR-APPLICATION that represents the
application of the LEFT term to the RIGHT term."
  (make-instance 'combinator-application :left left :right right))

(defun combinator-application-p (object)
  "Return true if OBJECT is a COMBINATOR-APPLICATION, and NIL
 otherwise."
  (typep object 'combinator-application))

(defclass combinator-variable (variable combinator-term) ()
  (:documentation "A variable for combinatory logic."))

(defun make-combinator-variable (name)
  "Construct and return a COMBINATOR-VARIABLE with name NAME."
  (make-instance 'combinator-variable :name name))

(defun combinator-variable-p (object)
  "Return true if OBJECT is a COMBINATOR-VARIABLE, and NIL otherwise."
  (typep object 'combinator-variable))

(defmethod term-equal ((term1 term) (term2 term))
  nil)

(defmethod term-equal ((term1 combinator) (term2 combinator))
  (eql term1 term2))

(defmethod term-equal ((term1 combinator-variable)
                       (term2 combinator-variable))
  (same-variable-p term1 term2))

(defmethod term-equal ((term1 combinator-application)
                       (term2 combinator-application))
  (with-accessors ((left1 application-left) (right1 application-right)) term1
    (with-accessors ((left2 application-left) (right2 application-right)) term2
      (and (term-equal left1 left2) (term-equal right1 right2)))))

(defmethod print-term ((term combinator)
                       &optional (stream *standard-output*))
  (format stream "~A" (combinator-name term))
  term)

(defmethod print-term ((term combinator-variable)
                       &optional (stream *standard-output*))
  (format stream "~A" (variable-name term))
  term)

(defmethod print-term ((term combinator-application)
                       &optional (stream *standard-output*))
  (with-accessors ((left application-left) (right application-right)) term
    (print-term left)
    (if (combinator-application-p right)
        (progn
          (write-char #\( stream)
          (print-term right)
          (write-char #\) stream))
        (print-term right)))
  term)

(defmethod occurs-free-p ((variable variable) (term combinator))
  nil)

(defgeneric step-combinator-term (term stack)
  (:documentation "Perform a step in the reduction of TERM, given the STACK of deferred
terms. Return the new term and the new stack."))

(defmethod step-combinator-term ((term combinator) (stack list))
  (values term stack))

(defmethod step-combinator-term ((term combinator-variable) (stack list))
  (values term stack))

(defmethod step-combinator-term ((term combinator-application) (stack list))
  (with-accessors ((left application-left) (right application-right)) term
    (values left (cons right stack))))

(defmethod reduce-term ((term combinator-term))
  (let ((next-term nil) (next-stack nil)
        (curr-term term) (curr-stack nil))
    (loop do (multiple-value-setq (next-term next-stack)
               (step-combinator-term curr-term curr-stack))
          until (and (eql curr-term next-term)
                     (eql curr-stack next-stack))
          do (setf curr-term next-term
                   curr-stack next-stack))
    (loop for stacked in curr-stack
          do (setf curr-term (make-combinator-application
                              curr-term
                              (reduce-term stacked))))
    curr-term))

(defparameter *combinators* (make-hash-table)
  "The table of interned combinators.")

(defun get-combinator (name)
  "Return the combinator whose name is NAME, and NIL if it doesn't exist."
  (gethash name *combinators*))

(defun intern-combinator (name combinator)
  "Store a combinator under the name NAME."
  (setf (gethash name *combinators*) combinator))

(defmacro define-combinator (name variables definition)
  "Define a new combinator called NAME that takes VARIABLES as arguments
and transforms its input as describe by DEFINITION."
  (let ((arity (length variables)))
    `(progn
       (intern-combinator ',name (make-combinator ',name ,arity))
       ,(expand-step-combinator-method name arity variables definition))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun expand-step-combinator-method (name arity variables definition)
    "Compute the DEFMETHOD form for a combinator given its NAME, VARIABLES
and DEFINITION."
    (labels ((expand-combinator-term (expr)
               (cond ((member expr variables)
                      expr)
                     ((symbolp expr)
                      `(get-combinator ',expr))
                     (t
                      (loop with curr = (expand-combinator-term (first expr))
                            for subexpr in (rest expr)
                            do (setf curr
                                     `(make-combinator-application
                                       ,curr
                                       ,(expand-combinator-term subexpr)))
                            finally (return curr)))))
             (expand-bindings (stack-variable)
               (loop for i from 0
                     for var in variables
                     collect `(,var (nth ,i ,stack-variable)))))
      (let ((stack-variable (gensym)))
        `(defmethod step-combinator-term ((term (eql (get-combinator ',name)))
                                          (,stack-variable list))
           (if (<= ,arity (length ,stack-variable))
               (let ,(expand-bindings stack-variable)
                 (declare (ignorable ,@variables))
                 (values ,(expand-combinator-term definition)
                         (nthcdr ,arity ,stack-variable)))
               (call-next-method)))))))

(define-combinator S (x y z) (x z (y z)))
(define-combinator S1 (x y z) (y z (x z)))
(define-combinator K (x y) x)
(define-combinator K2 (x y) y)
(define-combinator I (x) x)
(define-combinator I2 (x) (x I I))
(define-combinator B (x y z) (x (y z)))
(define-combinator B1 (x y z w) (x (y z w)))
(define-combinator B2 (x y z w v) (x (y z w v)))
(define-combinator B3 (x y z w) (x (y (z w))))
(define-combinator C (x y z) (x z y))
(define-combinator C* (x y z w) (x y w z))
(define-combinator C** (x y z w v) (x y z v w))
(define-combinator D (x y z w) (x y (z w)))
(define-combinator D1 (x y z w v) (x y z (w v)))
(define-combinator D2 (x y z w v) (x (y z) (w v)))
(define-combinator E (x y z w v) (x y (z w v)))
(define-combinator Ê (x y z w v u q) (x (y z w) (v u q)))
(define-combinator F (x y z) (z y x))
(define-combinator F* (x y z w) (x w z y))
(define-combinator F** (x y z w v) (x y v w z))
(define-combinator G (x y z w) (x w (y z)))
(define-combinator G1 (x y z w v) (x y v (z w)))
(define-combinator G2 (x y z w) (x w (x w) (y z)))
(define-combinator H (x y z) (x y z y))
(define-combinator H* (x y z w) (x y z w z))
(define-combinator J (x y z w) (x y (x w z)))
(define-combinator J1 (x y z w) (y x (w x z)))
(define-combinator L (x y) (x (y y)))
(define-combinator M (x) (x x))
(define-combinator M2 (x y) (x y (x y)))
(define-combinator O (x y) (y (x y)))
(define-combinator Q (x y z) (y (x z)))
(define-combinator Q1 (x y z) (x (z y)))
(define-combinator Q2 (x y z) (y (z x)))
(define-combinator Q3 (x y z) (z (x y)))
(define-combinator Q4 (x y z) (z (y x)))
(define-combinator R (x y z) (y z x))
(define-combinator R* (x y z w) (x z w y))
(define-combinator R** (x y z w v) (x y w v z))
(define-combinator Θ (x) (x (Θ x)))
(define-combinator T (x y) (y x))
(define-combinator U (x y) (y (x x y)))
(define-combinator V (x y z) (z x y))
(define-combinator V* (x y z w) (x w y z))
(define-combinator V** (x y z w v) (x y v z w))
(define-combinator W (x y) (x y y))
(define-combinator W1 (x y) (y x x))
(define-combinator W* (x y z) (x y z z))
(define-combinator W** (x y z w) (x y z w w))
(define-combinator Φ (x y z w) (x (y w) (z w)))
(define-combinator Ψ (x y z w) (x (y z) (y w)))
(define-combinator Γ (x y z w v) (y (z w) (x y w v)))
