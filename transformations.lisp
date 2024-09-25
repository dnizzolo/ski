(in-package #:ski)

(defgeneric lambda->ski (term)
  (:documentation "Convert a lambda calculus TERM to its corresponding SKI calculus term."))

(defmethod lambda->ski ((term (eql (get-combinator 'S))))
  term)

(defmethod lambda->ski ((term (eql (get-combinator 'K))))
  term)

(defmethod lambda->ski ((term (eql (get-combinator 'I))))
  term)

(defmethod lambda->ski ((term combinator-variable))
  term)

(defmethod lambda->ski ((term lambda-variable))
  (make-combinator-variable (name term)))

(defmethod lambda->ski ((term application))
  (with-accessors ((left left) (right right)) term
    (make-combinator-application (lambda->ski left) (lambda->ski right))))

(defmethod lambda->ski ((term lambda-abstraction))
  (with-accessors ((variable variable) (body body)) term
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
           (with-accessors ((left left) (right right)) body
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
  (:documentation "Convert a lambda calculus TERM to its corresponding SK calculus term."))

(defmethod lambda->sk ((term (eql (get-combinator 'S))))
  term)

(defmethod lambda->sk ((term (eql (get-combinator 'K))))
  term)

(defmethod lambda->sk ((term combinator-variable))
  term)

(defmethod lambda->sk ((term lambda-variable))
  (make-combinator-variable (name term)))

(defmethod lambda->sk ((term application))
  (with-accessors ((left left) (right right)) term
    (make-combinator-application (lambda->sk left) (lambda->sk right))))

(defmethod lambda->sk ((term lambda-abstraction))
  (with-accessors ((variable variable) (body body)) term
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
           (with-accessors ((left left) (right right)) body
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
  "Return a SKI calculus term equivalent to COMBINATOR."
  (labels ((eliminate (term var)
             (cond ((term-equal var term)
                    (get-combinator 'I))
                   ((not (occurs-free-p var term))
                    (make-combinator-application (get-combinator 'K) term))
                   ((and (combinator-application-p term)
                         (term-equal var (right term))
                         (not (occurs-free-p var (left term))))
                    (left term))
                   (t
                    (make-combinator-application
                     (make-combinator-application
                      (get-combinator 'S)
                      (eliminate (left term) var))
                     (eliminate (right term) var))))))
    (let* ((arity (arity combinator))
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

(defgeneric sk->goedel (term)
  (:documentation "Return the Gödel number of TERM."))

(defmethod sk->goedel ((term (eql (get-combinator 'S))))
  1)

(defmethod sk->goedel ((term (eql (get-combinator 'K))))
  2)

(defmethod sk->goedel ((term combinator-application))
  (nth-value
   0
   (parse-integer
    (concatenate
     'string
     "3"
     (write-to-string (sk->goedel (left term)))
     (write-to-string (sk->goedel (right term)))
     "4"))))

(defun goedel->sk (n)
  "Return the SK term denoted by the Gödel number N."
  (parse-combinator-term
   (nsubstitute
    #\S #\1
    (nsubstitute
     #\K #\2
     (nsubstitute
      #\( #\3
      (nsubstitute
       #\) #\4
       (write-to-string n)))))))

(defun natural->church (n)
  "Convert the natural number N to its representation as a Church
numeral."
  (declare (type (integer 0) n))
  (let ((f (make-lambda-variable #\f))
        (x (make-lambda-variable #\x)))
    (make-lambda-abstraction
     f
     (make-lambda-abstraction
      x
      (let ((acc x))
        (dotimes (i n acc)
          (setf acc (make-lambda-application f acc))))))))

(defun church->natural (term)
  "Convert the Church numeral TERM to its corresponding natural number."
  (do ((result 0)
       (body (body (body term))))
      ((lambda-variable-p body) result)
    (incf result)
    (setf body (right body))))

(defun natural->barendregt (n)
  "Convert the natural number N to its representation in the scheme used
in the book To Mock a Mockingbird by Raymond Smullyan."
  (declare (type (integer 0) n))
  (let* ((v (get-combinator 'V))
         (z (get-combinator 'I))
         (f (make-combinator-application (get-combinator 'K) z))
         (succ (make-combinator-application v f))
         (result z))
    (dotimes (i n result)
      (setf result (make-combinator-application succ result)))))

(defun barendregt->natural (term)
  "Convert a numeral from the scheme used in the book To Mock a
  Mockingbird by Raymond Smullyan to its corresponding natural number."
  (do ((i (get-combinator 'I))
       (result 0)
       (acc term))
      ((term-equal i acc) result)
    (incf result)
    (setf acc (right acc))))
