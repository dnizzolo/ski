(in-package #:ski)

(defun print-help-and-exit (&optional (code 0))
  (format t "Usage: ski <subcommand>...~%~%~
Available subcommands:
  lambda-repl                            Start a lambda calculus REPL.
  combinator-repl                        Start a combinatory logic REPL.
  lambda-program <file>                  Evaluate the lambda program.
  combinator-program <file>              Evaluate the combinatory logic program.~%")
  (uiop:quit code))

(defun toplevel ()
  (flet ((run-program (file function)
           (let ((path (uiop:file-exists-p file)))
             (if path
                 (funcall function path)
                 (format t "File ~a does not exist~%" file)))))
    (let* ((argv (uiop:command-line-arguments))
           (argc (length argv)))
      (case argc
        (1 (let ((subcommand (first argv)))
             (cond ((string= subcommand "lambda-repl")
                    (lambda-driver-loop))
                   ((string= subcommand "combinator-repl")
                    (combinator-driver-loop))
                   (t (print-help-and-exit)))))
        (2 (let ((subcommand (first argv))
                 (operand (second argv)))
             (run-program
              operand
              (cond ((string= subcommand "lambda-program")
                     #'run-lambda-program)
                    ((string= subcommand "combinator-program")
                     #'run-combinator-program)
                    (t (print-help-and-exit))))))
        (otherwise (print-help-and-exit))))))
