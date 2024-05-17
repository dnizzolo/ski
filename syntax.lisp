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
