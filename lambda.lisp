(in-package #:ski)

(defclass lambda-term (term) ()
  (:documentation "The base class for a lambda calculus term."))

(defun lambda-term-p (object)
  "Return true if OBJECT is a LAMBDA-TERM, and NIL otherwise."
  (typep object 'lambda-term))

(defclass lambda-abstraction (lambda-term)
  ((variable :initarg :variable :accessor variable)
   (body :initarg :body :accessor body))
  (:documentation "A lambda abstraction in lambda calculus."))

(defun make-lambda-abstraction (variable body)
  "Construct and return a LAMBDA-ABSTRACTION with VARIABLE and BODY."
  (make-instance 'lambda-abstraction :variable variable :body body))

(defun lambda-abstraction-p (object)
  "Return true if OBJECT is a LAMBDA-ABSTRACTION, and NIL otherwise."
  (typep object 'lambda-abstraction))

(defmethod print-object ((object lambda-abstraction) stream)
  (with-slots (variable body) object
    (print-unreadable-object (object stream :type t :identity t)
      (format stream "(~a ~a)" variable body))))

(defmethod print-term ((term lambda-abstraction) &optional (stream *standard-output*))
  (write-char #\λ stream)
  (print-term (variable term) stream)
  (write-char #\. stream)
  (print-term (body term) stream)
  term)

(defclass lambda-application (application lambda-term) ()
  (:documentation "An application in lambda calculus."))

(defun make-lambda-application (left right)
  "Construct and return a LAMBDA-APPLICATION that represents the
application of the LEFT term to the RIGHT term."
  (make-instance 'lambda-application :left left :right right))

(defun lambda-application-p (object)
  "Return true if OBJECT is a LAMBDA-APPLICATION, and NIL otherwise."
  (typep object 'lambda-application))

(defmethod print-term ((term lambda-application) &optional (stream *standard-output*))
  (with-accessors ((left left) (right right)) term
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

(defclass lambda-variable (variable lambda-term) ()
  (:documentation "A variable in the lambda calculus."))

(defun make-lambda-variable (name)
  "Construct and return a LAMBDA-VARIABLE."
  (make-instance 'lambda-variable :name name))

(defun lambda-variable-p (object)
  "Return true if OBJECT is a LAMBDA-VARIABLE, and NIL otherwise."
  (typep object 'lambda-variable))

(defgeneric alpha-equivalent-p (term1 term2 env)
  (:documentation "Return true if TERM1 and TERM2 are alpha-equivalent lambda calculus
terms, and NIL otherwise. Use the environment ENV to resolve the
bindings of variables."))

(defmethod alpha-equivalent-p ((term1 lambda-term) (term2 lambda-term) env)
  nil)

(defmethod alpha-equivalent-p ((term1 lambda-variable) (term2 lambda-variable) env)
  (let ((assoc1 (cdr (assoc term1 env :test #'same-variable-p))))
    (cond (assoc1 (same-variable-p assoc1 term2))
          ((rassoc term2 env :test #'same-variable-p) nil)
          (t (same-variable-p term1 term2)))))

(defmethod alpha-equivalent-p ((term1 lambda-application) (term2 lambda-application) env)
  (and
   (alpha-equivalent-p (left term1) (left term2) env)
   (alpha-equivalent-p (right term1) (right term2) env)))

(defmethod alpha-equivalent-p ((term1 lambda-abstraction) (term2 lambda-abstraction) env)
  (alpha-equivalent-p
   (body term1)
   (body term2)
   (acons (variable term1) (variable term2) env)))

(defmethod term-equal ((term1 lambda-term) (term2 lambda-term))
  (alpha-equivalent-p term1 term2 nil))

(defmethod occurs-free-p ((variable variable) (term lambda-abstraction))
  (unless (same-variable-p variable (variable term))
    (occurs-free-p variable (body term))))

(defgeneric free-variables (term)
  (:documentation "Return the free variables in the lambda calculus TERM."))

(defmethod free-variables ((term lambda-variable))
  (list term))

(defmethod free-variables ((term lambda-application))
  (with-accessors ((left left) (right right)) term
    (union (free-variables left)
           (free-variables right)
           :test #'same-variable-p)))

(defmethod free-variables ((term lambda-abstraction))
  (with-accessors ((variable variable) (body body)) term
    (delete-if (lambda (fv) (same-variable-p fv variable))
               (free-variables body))))

(defgeneric bound-variables (term)
  (:documentation "Return the bound variables in the lambda calculus TERM."))

(defmethod bound-variables ((term lambda-variable))
  nil)

(defmethod bound-variables ((term lambda-application))
  (with-accessors ((left left) (right right)) term
    (union (bound-variables left)
           (bound-variables right)
           :test #'same-variable-p)))

(defmethod bound-variables ((term lambda-abstraction))
  (with-accessors ((variable variable) (body body)) term
    (union (list variable)
           (bound-variables body)
           :test #'same-variable-p)))

(defgeneric lambda-combinator-p (term)
  (:documentation "Return true if TERM is a lambda calculus combinator, and NIL
otherwise."))

(defmethod lambda-combinator-p ((term lambda-term))
  nil)

(defmethod lambda-combinator-p ((term lambda-abstraction))
  (null (free-variables term)))

(defgeneric substitute-avoiding-capture (term target replacement)
  (:documentation "Return a new term where the free occurrences of the variable TARGET in
TERM have been replaced with REPLACEMENT without capturing free
variables while doing so."))

(defmethod substitute-avoiding-capture ((term lambda-variable)
                                        (target lambda-variable)
                                        (replacement lambda-term))
  (if (same-variable-p term target) replacement term))

(defmethod substitute-avoiding-capture ((term lambda-application)
                                        (target lambda-variable)
                                        (replacement lambda-term))
  (with-accessors ((left left) (right right)) term
    (make-lambda-application
     (substitute-avoiding-capture left target replacement)
     (substitute-avoiding-capture right target replacement))))

(defmethod substitute-avoiding-capture ((term lambda-abstraction)
                                        (target lambda-variable)
                                        (replacement lambda-term))
  (flet ((fresh-variable (t1 t2)
           (let ((forbidden (union (free-variables t1)
                                   (free-variables t2)
                                   :test #'same-variable-p))
                 (generator (make-variable-name-generator)))
             (loop
               (let ((var (make-lambda-variable (generate-name generator))))
                 (unless (member var forbidden :test #'same-variable-p)
                   (return var)))))))
    (with-accessors ((variable variable) (body body)) term
      (cond ((same-variable-p variable target)
             term)
            ((not (occurs-free-p target body))
             term)
            ((not (occurs-free-p variable replacement))
             (make-lambda-abstraction
              variable
              (substitute-avoiding-capture body target replacement)))
            (t
             (let ((fresh-variable (fresh-variable body replacement)))
               (make-lambda-abstraction
                fresh-variable
                (substitute-avoiding-capture
                 (substitute-avoiding-capture
                  body
                  variable
                  fresh-variable)
                 target
                 replacement))))))))

(defvar *lambda-reduction-strategy* :normal-order
  "The reduction strategy to use when reducing lambda calculus terms.
Must be a one of the following keywords: :NORMAL-ORDER,
:APPLICATIVE-ORDER, :CALL-BY-NAME, :CALL-BY-VALUE.")

(defgeneric beta-reduce (term strategy)
  (:documentation "Perform a beta reduction on TERM using the reduction STRATEGY. Return,
as multiple values, the new term and a generalized boolean that is
true if a step took place and NIL otherwise."))

(defmethod beta-reduce ((term lambda-variable) strategy)
  (values term nil))

(defmethod beta-reduce ((term lambda-abstraction) (strategy (eql :normal-order)))
  (multiple-value-bind (new-body stepped)
      (beta-reduce (body term) strategy)
    (values (make-lambda-abstraction (variable term) new-body)
            stepped)))

(defmethod beta-reduce ((term lambda-application) (strategy (eql :normal-order)))
  (with-accessors ((left left) (right right)) term
    (if (lambda-abstraction-p left)
        (values (substitute-avoiding-capture
                 (body left)
                 (variable left)
                 right)
                t)
        (multiple-value-bind (new-left stepped-left)
            (beta-reduce left strategy)
          (multiple-value-bind (new-right stepped-right)
              (beta-reduce right strategy)
            (values (make-lambda-application new-left new-right)
                    (or stepped-left stepped-right)))))))

(defmethod beta-reduce ((term lambda-abstraction) (strategy (eql :applicative-order)))
  (multiple-value-bind (new-body stepped)
      (beta-reduce (body term) strategy)
    (values (make-lambda-abstraction (variable term) new-body)
            stepped)))

(defmethod beta-reduce ((term lambda-application) (strategy (eql :applicative-order)))
  (with-accessors ((left left) (right right)) term
    (if (lambda-abstraction-p left)
        (values (substitute-avoiding-capture
                 (reduce-term (body left))
                 (variable left)
                 (reduce-term right))
                t)
        (multiple-value-bind (new-left stepped-left)
            (beta-reduce left strategy)
          (multiple-value-bind (new-right stepped-right)
              (beta-reduce right strategy)
            (values (make-lambda-application new-left new-right)
                    (or stepped-left stepped-right)))))))

(defmethod beta-reduce ((term lambda-abstraction) (strategy (eql :call-by-name)))
  (values term nil))

(defmethod beta-reduce ((term lambda-application) (strategy (eql :call-by-name)))
  (with-accessors ((left left) (right right)) term
    (if (lambda-abstraction-p left)
        (values (substitute-avoiding-capture
                 (body left)
                 (variable left)
                 right)
                t)
        (multiple-value-bind (new-left stepped-left)
            (beta-reduce left strategy)
          (multiple-value-bind (new-right stepped-right)
              (beta-reduce right strategy)
            (values (make-lambda-application new-left new-right)
                    (or stepped-left stepped-right)))))))

(defmethod beta-reduce ((term lambda-abstraction) (strategy (eql :call-by-value)))
  (values term nil))

(defmethod beta-reduce ((term lambda-application) (strategy (eql :call-by-value)))
  (with-accessors ((left left) (right right)) term
    (if (lambda-abstraction-p left)
        (values (substitute-avoiding-capture
                 (body left)
                 (variable left)
                 (reduce-term right))
                t)
        (multiple-value-bind (new-left stepped-left)
            (beta-reduce left strategy)
          (multiple-value-bind (new-right stepped-right)
              (beta-reduce right strategy)
            (values (make-lambda-application new-left new-right)
                    (or stepped-left stepped-right)))))))

(defmethod reduce-term ((term lambda-term))
  (loop
    (multiple-value-bind (new-term stepped)
        (beta-reduce term *lambda-reduction-strategy*)
      (unless stepped
        (return term))
      (setf term new-term))))

(defun lambda-driver-loop ()
  "A REPL for lambda calculus."
  (flet ((prompt-for-input ()
           (format t "~&%%% ")
           (finish-output)
           (read-line t nil))
         (drive (input)
           (print-term (reduce-term (parse-lambda-term input)))))
    (loop
      (let ((input (prompt-for-input)))
        (cond ((null input) (return))
              ((zerop (length input)))
              (t (handler-case (drive input)
                   (esrap:esrap-parse-error ()
                     (format t "~&Parse error")))))))))
