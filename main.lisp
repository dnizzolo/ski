(in-package #:ski)

(defun print-help ()
  (format t "Usage: ski <subcommand>...~%~%~
Available subcommands:
  lambda-repl                            Start a lambda calculus REPL.
  combinator-repl                        Start a combinatory logic REPL.
  lambda-program <file>                  Evaluate the lambda program.
  combinator-program <file>              Evaluate the combinatory logic program.~%"))

(defun toplevel ()
  (flet ((run-program (file function)
           (cond ((null file)
                  (format t "No file was specified.~%"))
                 ((probe-file file)
                  (handler-case
                      (funcall function (pathname file))
                    (type-error ()
                      (format t "~a does not designate a file~%." file))))
                 (t (format t "File ~a does not exist.~%" file)))))
    (multiple-value-bind (subcommand operand)
        (values-list (uiop:command-line-arguments))
      (cond ((string= subcommand "lambda-repl")
             (lambda-driver-loop))
            ((string= subcommand "combinator-repl")
             (combinator-driver-loop))
            ((string= subcommand "lambda-program")
             (run-program operand #'run-lambda-program))
            ((string= subcommand "combinator-program")
             (run-program operand #'run-combinator-program))
            (t (print-help))))))
