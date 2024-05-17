(in-package #:ski)

(defgeneric lambda->ski (term)
  (:documentation "Convert TERM to its corresponding COMBINATOR-TERM which uses only the
S, K and I combinators."))

(defmethod lambda->ski ((term (eql (get-combinator 'S))))
  term)

(defmethod lambda->ski ((term (eql (get-combinator 'K))))
  term)

(defmethod lambda->ski ((term (eql (get-combinator 'I))))
  term)

(defmethod lambda->ski ((term combinator-variable))
  term)

(defmethod lambda->ski ((term lambda-variable))
  (make-combinator-variable (variable-name term)))

(defmethod lambda->ski ((term application))
  (with-accessors ((left application-left)
                   (right application-right))
      term
    (make-combinator-application (lambda->ski left) (lambda->ski right))))

(defmethod lambda->ski ((term lambda-abstraction))
  (with-accessors ((variable lambda-abstraction-variable)
                   (body lambda-abstraction-body))
      term
    (cond ((not (occurs-free-p variable body))
           (make-combinator-application
            (get-combinator 'K)
            (lambda->ski body)))
          ((and (variable-p body) (same-variable-p variable body))
           (get-combinator 'I))
          ((lambda-abstraction-p body)
           (lambda->ski
            (make-lambda-abstraction variable (lambda->ski body))))
          ((application-p body)
           (with-accessors ((left application-left)
                            (right application-right))
               body
             (if (and (variable-p right)
                      (same-variable-p right variable)
                      (not (occurs-free-p variable left)))
                 (lambda->ski left)
                 (make-combinator-application
                  (make-combinator-application
                   (get-combinator 'S)
                   (lambda->ski (make-lambda-abstraction variable left)))
                  (lambda->ski (make-lambda-abstraction variable right)))))))))

(defgeneric lambda->sk (term)
  (:documentation "Convert TERM to its corresponding COMBINATOR-TERM which uses only the
S and K combinators."))

(defmethod lambda->sk ((term (eql (get-combinator 'S))))
  term)

(defmethod lambda->sk ((term (eql (get-combinator 'K))))
  term)

(defmethod lambda->sk ((term combinator-variable))
  term)

(defmethod lambda->sk ((term lambda-variable))
  (make-combinator-variable (variable-name term)))

(defmethod lambda->sk ((term application))
  (with-accessors ((left application-left)
                   (right application-right))
      term
    (make-combinator-application (lambda->sk left) (lambda->sk right))))

(defmethod lambda->sk ((term lambda-abstraction))
  (with-accessors ((variable lambda-abstraction-variable)
                   (body lambda-abstraction-body))
      term
    (cond ((not (occurs-free-p variable body))
           (make-combinator-application
            (get-combinator 'K)
            (lambda->sk body)))
          ((and (variable-p body) (same-variable-p variable body))
           (make-combinator-application
            (make-combinator-application
             (get-combinator 'S)
             (get-combinator 'K))
            (get-combinator 'K)))
          ((lambda-abstraction-p body)
           (lambda->sk
            (make-lambda-abstraction variable (lambda->sk body))))
          ((application-p body)
           (with-accessors ((left application-left)
                            (right application-right))
               body
             (if (and (variable-p right)
                      (same-variable-p right variable)
                      (not (occurs-free-p variable left)))
                 (lambda->sk left)
                 (make-combinator-application
                  (make-combinator-application
                   (get-combinator 'S)
                   (lambda->sk (make-lambda-abstraction variable left)))
                  (lambda->sk (make-lambda-abstraction variable right)))))))))

(defun combinator->ski (combinator)
  "Return a term equivalent to COMBINATOR using only the S, K and I
combinators."
  (labels ((eliminate (term var)
             (cond ((term-equal var term)
                    (get-combinator 'I))
                   ((not (occurs-free-p var term))
                    (make-combinator-application (get-combinator 'K) term))
                   ((and (combinator-application-p term)
                         (term-equal var (application-right term))
                         (not (occurs-free-p var (application-left term))))
                    (application-left term))
                   (t
                    (make-combinator-application
                     (make-combinator-application
                      (get-combinator 'S)
                      (eliminate (application-left term) var))
                     (eliminate (application-right term) var))))))
    (let* ((arity (combinator-arity combinator))
           (vars (loop with g = (make-variable-name-generator)
                       repeat arity
                       collect (make-combinator-variable (generate-name g)))))
      (let ((term (reduce-term
                   (reduce #'make-combinator-application
                           vars
                           :initial-value combinator))))
        (loop for var in (nreverse vars)
              do (setf term (eliminate term var))
              finally (return term))))))
