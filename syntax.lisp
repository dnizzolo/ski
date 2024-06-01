(in-package #:ski)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Common rules.

(esrap:defrule whitespace*
    (* (or #\Space #\Newline #\Tab))
  (:constant nil))

(esrap:defrule variable
    (and whitespace*
         (or (esrap:character-ranges (#\a #\z))
             (and "{"
                  whitespace*
                  (+ (esrap:character-ranges (#\a #\z)))
                  whitespace*
                  "}"))
         whitespace*)
  (:destructure (w1 var w2)
    (declare (ignore w1 w2))
    (if (atom var) var (third var))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Combinatory logic grammar.

(esrap:defrule combinator-term
    combinator-expr)

(esrap:defrule combinator-expr
    (and (esrap:? combinator-expr) combinator-factor)
  (:destructure (left right)
    (if left
        (make-combinator-application left right)
        right)))

(esrap:defrule combinator-factor
    (or combinator
        combinator-variable
        parenthesized-combinator-term))

(esrap:defrule combinator
    (and whitespace*
         (or "Q1"
             "Q2"
             "Q3"
             "Q4"
             "Q"
             "B1"
             "B2"
             "B3"
             "D1"
             "D2"
             "I2"
             "G1"
             "G2"
             "K2"
             "J1"
             "M2"
             "R**"
             "R*"
             "R"
             "S1"
             "F**"
             "F*"
             "H*"
             "C**"
             "C*"
             "W**"
             "W*"
             "w1"
             "V**"
             "V*"
             "Θ"
             "Ψ"
             "Φ"
             "Γ"
             "Ê"
             "O"
             (esrap:character-ranges (#\B #\M) (#\S #\W)))
         whitespace*)
  (:destructure (w1 combinator w2)
    (declare (ignore w1 w2))
    (get-combinator (intern (string combinator)))))

(esrap:defrule combinator-variable
    variable
  (:function make-combinator-variable))

(esrap:defrule combinator-application
    (and combinator-term (or combinator combinator-variable combinator-term))
  (:destructure (left right)
    (make-combinator-application left right)))

(esrap:defrule parenthesized-combinator-term
    (and whitespace*
         "("
         combinator-term
         ")"
         whitespace*)
  (:destructure (w1 open-parenthesis term close-parenthesis w2)
    (declare (ignore w1 open-parenthesis close-parenthesis w2))
    term))

(defun parse-combinator-term (input)
  "Parse a COMBINATOR-TERM from the string INPUT and return it."
  (esrap:parse 'combinator-term input))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Lambda calculus grammar.

(esrap:defrule lambda-term
    lambda-expr)

(esrap:defrule lambda-expr
    (and (esrap:? lambda-expr) lambda-factor)
  (:destructure (left right)
    (if left
        (make-lambda-application left right)
        right)))

(esrap:defrule lambda-factor
    (or lambda-abstraction
        lambda-variable
        parenthesized-lambda-term))

(esrap:defrule lambda-abstraction
    (and whitespace*
         "λ"
         (+ lambda-variable)
         "."
         lambda-term
         whitespace*)
  (:destructure (w1 lambda variables dot body w2)
    (declare (ignore w1 lambda dot w2))
    (reduce #'make-lambda-abstraction
            variables
            :initial-value body
            :from-end t)))

(esrap:defrule lambda-variable
    variable
  (:function make-lambda-variable))

(esrap:defrule parenthesized-lambda-term
    (and whitespace*
         "("
         lambda-term
         ")"
         whitespace*)
  (:destructure (w1 open-parenthesis term close-parenthesis w2)
    (declare (ignore w1 open-parenthesis close-parenthesis w2))
    term))

(defun parse-lambda-term (input)
  "Parse a LAMBDA-TERM from the string INPUT and return it."
  (esrap:parse 'lambda-term input))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Lambda calculus programs.

(esrap:defrule lambda-program
    (and (* lambda-definition)
         (+ (and lambda-program-expr ";" whitespace*)))
  (:destructure (definitions exprs)
    (list (coerce definitions 'vector)
          (mapcar #'first exprs))))

(esrap:defrule lambda-name
    (and whitespace*
         (+ (esrap:character-ranges (#\A #\Z)))
         whitespace*)
  (:text t))

(esrap:defrule lambda-definition
    (and whitespace*
         lambda-name
         whitespace*
         "="
         whitespace*
         lambda-program-expr
         whitespace*
         ";"
         whitespace*)
  (:destructure (w1 name w2 eq w3 expr w4 semicolon w5)
    (declare (ignore w1 w2 eq w3 w4 semicolon w5))
    (cons (coerce name 'string) expr)))

(esrap:defrule lambda-program-expr
    (and (esrap:? lambda-program-expr) lambda-program-factor)
  (:destructure (left right)
    (if left
        (make-lambda-application left right)
        right)))

(esrap:defrule lambda-program-factor
    (or lambda-program-abstraction
        lambda-variable
        lambda-name
        parenthesized-lambda-program-expr))

(esrap:defrule lambda-program-abstraction
    (and whitespace*
         "λ"
         (+ lambda-variable)
         "."
         lambda-program-expr
         whitespace*)
  (:destructure (w1 lambda variables dot body w2)
    (declare (ignore w1 lambda dot w2))
    (reduce #'make-lambda-abstraction
            variables
            :initial-value body
            :from-end t)))

(esrap:defrule parenthesized-lambda-program-expr
    (and whitespace*
         "("
         lambda-program-expr
         ")"
         whitespace*)
  (:destructure (w1 open-parenthesis term close-parenthesis w2)
    (declare (ignore w1 open-parenthesis close-parenthesis w2))
    term))

(defgeneric parse-lambda-program (input)
  (:documentation "Parse a lambda program from INPUT."))

(defmethod parse-lambda-program ((input string))
  (esrap:parse 'lambda-program input))

(defmethod parse-lambda-program ((input pathname))
  (parse-lambda-program (uiop:read-file-string input)))

(defgeneric lambda-names (term)
  (:documentation "Return all unresolved names in TERM."))

(defmethod lambda-names ((term string))
  (list term))

(defmethod lambda-names ((term lambda-variable))
  nil)

(defmethod lambda-names ((term lambda-abstraction))
  (lambda-names (lambda-abstraction-body term)))

(defmethod lambda-names ((term lambda-application))
  (append (lambda-names (application-left term))
          (lambda-names (application-right term))))

(defgeneric substitute-definitions (definitions term)
  (:documentation "Recursively replace all occurrences of names in TERM with their
definition in DEFINITIONS."))

(defmethod substitute-definitions (definitions (term lambda-variable))
  term)

(defmethod substitute-definitions (definitions (term string))
  (substitute-definitions
   definitions
   (cdr (find term definitions :key #'car :test #'string=))))

(defmethod substitute-definitions (definitions (term lambda-abstraction))
  (with-accessors ((variable lambda-abstraction-variable)
                   (body lambda-abstraction-body))
      term
    (make-lambda-abstraction
     variable
     (substitute-definitions definitions body))))

(defmethod substitute-definitions (definitions (term lambda-application))
  (with-accessors ((left application-left) (right application-right)) term
    (make-lambda-application
     (substitute-definitions definitions left)
     (substitute-definitions definitions right))))

(defun build-lambda-program (program)
  "Parse the lambda PROGRAM and return a list of pure lambda calculus
terms to be reduced."
  (flet ((build (definitions expressions)
           (dotimes (i (length definitions))
             (let* ((curr-expr (cdr (aref definitions i)))
                    (items (lambda-names curr-expr)))
               (dolist (item items)
                 (or (dotimes (j i)
                       (when (string= item (car (aref definitions j)))
                         (return t)))
                     (error "Definition of ~A contains undefined term ~A"
                            (car (aref definitions i))
                            item)))))
           (mapcar (lambda (expr)
                     (substitute-definitions definitions expr))
                   expressions)))
    (multiple-value-call #'build (values-list (parse-lambda-program program)))))

(defun run-lambda-program (program &optional (stream *standard-output*))
  "Parse and run the lambda PROGRAM. Return the last reduced term. Print
each reduced term to STREAM."
  (let (last)
    (dolist (expr (build-lambda-program program) last)
      (fresh-line)
      (setf last (reduce-term expr))
      (print-term last stream))))
