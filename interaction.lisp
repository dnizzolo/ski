(in-package #:ski)

(defvar *bindings*)

(defmethod step-combinator-term :around ((term combinator-variable) (stack list))
  (if (boundp '*bindings*)
      (multiple-value-bind (binding found-p) (gethash (name term) *bindings*)
        (if found-p (funcall (operation binding) stack) (call-next-method)))
      (call-next-method)))

(defvar *commands* (make-hash-table)
  "Table of available commands.")

(defgeneric print-command (object)
  (:documentation "Show a command to the user."))

(defmacro define-command (name args)
  (multiple-value-bind (slots initkeys)
      (loop for arg in args
            for str = (string arg)
            for key = (intern str :keyword)
            collect `(,arg :initarg ,key :reader ,arg) into slots
            collect key into initkeys
            finally (return (values slots initkeys)))
    `(progn
       (setf (gethash ,(intern (string name) :keyword) *commands*)
             (lambda ,args
               (make-instance ',name ,@(loop for key in initkeys
                                             for arg in args
                                             nconc (list key arg)))))
       (defclass ,name ()
         ,slots)
       (defmethod print-command ((object ,name))
         (let ((*print-case* :downcase))
           (format t ":~a ~{<~a>~^ ~}" ',name ',args))))))

(define-command load-bindings-file (file))
(define-command write-bindings-file (file))
(define-command print-bindings ())
(define-command delete-binding (name))
(define-command print-commands ())

(defun parse-command (source)
  (multiple-value-bind (command args)
      (ignore-errors
       (let* ((parts (split-whitespace source))
              (command (read-from-string (first parts))))
         (values command (rest parts))))
    (multiple-value-bind (constructor found-p) (gethash command *commands*)
      (when found-p
        (handler-case (apply constructor args)
          (error ()
            (error "Bad arguments for command :~a" (string-downcase (string command)))))))))

(defclass repl () ())
(defclass lambda-repl (repl) ())
(defclass combinator-repl (repl) ())

(defgeneric execute-in-repl (repl object)
  (:documentation "Execute OBJECT in the context of REPL."))

(defmethod execute-in-repl ((repl combinator-repl) (object combinator-binding))
  (let* ((name (name object))
         (present-p (nth-value 1 (gethash name *bindings*))))
    (setf (gethash name *bindings*) object)
    (format t "~&~:[Created~;Updated~] binding ~a." present-p name)))

(defmethod execute-in-repl ((repl lambda-repl) (object lambda-binding))
  (let* ((name (name object))
         (maybe-i (position name *bindings* :key #'name :test #'string=)))
    (if maybe-i
        (setf (aref *bindings* maybe-i) object)
        (vector-push-extend object *bindings*))
    (format t "~&~:[Created~;Updated~] binding ~a." maybe-i name)))

(defmethod execute-in-repl ((repl combinator-repl) (object combinator-term))
  (print-term (reduce-term object)))

(defmethod execute-in-repl ((repl lambda-repl) (object lambda-term))
  (let ((subbed (substitute-lambda-bindings object *bindings*)))
    (print-term (reduce-term subbed))))

(defmethod execute-in-repl ((repl lambda-repl) (object print-bindings))
  (loop for binding across *bindings*
        do (fresh-line)
           (print-term binding)))

(defmethod execute-in-repl ((repl combinator-repl) (object print-bindings))
  (loop for binding being the hash-values of *bindings*
        do (fresh-line)
           (print-term binding)))

(defmethod execute-in-repl ((repl combinator-repl) (object write-bindings-file))
  (with-open-file (out (file object) :direction :output
                                     :if-exists :overwrite)
    (loop for binding being the hash-values of *bindings*
          do (fresh-line out)
             (print-term binding out)
             (write-char #\; out))))

(defmethod execute-in-repl ((repl lambda-repl) (object write-bindings-file))
  (with-open-file (out (file object) :direction :output
                                     :if-exists :overwrite)
    (loop for binding across *bindings*
          do (fresh-line out)
             (print-term binding out)
             (write-char #\; out))))

(defmethod execute-in-repl ((repl combinator-repl) (object load-bindings-file))
  (let* ((toplevels (parse-combinator-file (file object)))
         (bindings (remove-if-not #'combinator-binding-p toplevels)))
    (loop for binding across bindings
          do (execute-in-repl repl binding))))

(defmethod execute-in-repl ((repl lambda-repl) (object load-bindings-file))
  (let* ((toplevels (parse-lambda-file (file object)))
         (bindings (remove-if-not #'lambda-binding-p toplevels)))
    (loop for binding across bindings
          do (execute-in-repl repl binding))))

(defmethod execute-in-repl ((repl combinator-repl) (object delete-binding))
  (remhash (name object) *bindings*))

(defmethod execute-in-repl ((repl lambda-repl) (object delete-binding))
  (let ((maybe-i (position (name object) *bindings* :key #'name :test #'string=)))
    (when maybe-i
      (loop for i from maybe-i below (1- (length *bindings*))
            do (rotatef (aref *bindings* i) (aref *bindings* (1+ i)))
            finally (return (vector-pop *bindings*))))))

(defmethod execute-in-repl ((repl repl) (object print-commands))
  (loop for command being the hash-keys of *commands*
        for class = (intern (string command))
        for instance = (make-instance class)
        do (fresh-line) (print-command instance)))

(defun prompt-for-input ()
  (format t "~&>>> ")
  (finish-output)
  (read-line t nil))

(defmacro with-repl-loop ((line-variable) repl &body body)
  (let ((condition-variable (gensym)) (command-variable (gensym)))
    `(loop for ,line-variable = (prompt-for-input)
           while ,line-variable
           when (plusp (length ,line-variable))
             do (handler-case
                    (let ((,command-variable (parse-command ,line-variable)))
                      (if ,command-variable
                          (execute-in-repl ,repl ,command-variable)
                          (progn ,@body)))
                  (error (,condition-variable) (princ ,condition-variable))))))

(defgeneric run-repl (repl))

(defmethod run-repl ((repl lambda-repl))
  (let ((*bindings* (make-array 0 :adjustable t :fill-pointer t)))
    (with-repl-loop (input) repl
      (let ((thing (parse-lambda-input input)))
        (execute-in-repl repl thing)))))

(defmethod run-repl ((repl combinator-repl))
  (let ((*bindings* (make-hash-table :test #'equal)))
    (with-repl-loop (input) repl
      (let ((thing (parse-combinator-input input)))
        (execute-in-repl repl thing)))))

(defun run-lambda-repl ()
  "A REPL for lambda calculus."
  (run-repl (make-instance 'lambda-repl)))

(defun run-combinator-repl ()
  "A REPL for combinatory logic."
  (run-repl (make-instance 'combinator-repl)))

(defun substitute-lambda-bindings (term bindings)
  (loop for i from (1- (length bindings)) downto 0
        for binding = (aref bindings i)
        for var = (make-lambda-variable (name binding))
        for sub = (term binding)
        do (setf term (substitute-avoiding-capture term var sub))
        finally (return term)))

(defun reduce-and-print-terms (terms &optional (stream *standard-output*))
  (let (last)
    (map nil (lambda (term)
               (setf last (reduce-term term))
               (fresh-line stream)
               (print-term last stream))
         terms)
    last))

(defun run-lambda-program (file &optional (stream *standard-output*))
  "Parse FILE and collect all lambda bindings and terms. Evaluate the
terms in an environment that contains the bindings, printing each result to STREAM."
  (let* ((toplevels (parse-lambda-file file))
         (terms (remove-if #'lambda-binding-p toplevels))
         (bindings (remove-if-not #'lambda-binding-p toplevels)))
    (map-into terms (lambda (term) (substitute-lambda-bindings term bindings)) terms)
    (reduce-and-print-terms terms stream)))

(defun run-combinator-program (file &optional (stream *standard-output*))
  "Parse FILE and collect all combinator bindings and terms. Evaluate the
terms in an environment that contains the bindings, printing each result to STREAM."
  (let* ((toplevels (parse-combinator-file file))
         (terms (remove-if #'combinator-binding-p toplevels))
         (*bindings*
           (loop with table = (make-hash-table :test #'equal)
                 for binding across (remove-if-not #'combinator-binding-p toplevels)
                 do (setf (gethash (name binding) table) binding)
                 finally (return table))))
    (reduce-and-print-terms terms stream)))
