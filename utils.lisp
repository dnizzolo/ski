(in-package #:ski)

(defun split-whitespace (string &key (start 0) (end (length string)))
  (let ((whitespace '(#\Space #\Tab #\Return #\Page #\Vt #\No-break_space))
        (out (make-string-output-stream))
        pieces)
    (flet ((add ()
             (let ((string (get-output-stream-string out)))
               (unless (string= string "")
                 (push string pieces)))))
      (loop for i from start below end
            for c = (char string i)
            do (if (member c whitespace) (add) (write-char c out))
            finally (add)))
    (nreverse pieces)))

(defun latin-lower-case-p (char)
  (<= #.(char-code #\a) (char-code char) #.(char-code #\z)))

(defun extract-line (source line-number)
  (loop for i from 1
        for start = 0 then (1+ end)
        for end = (position #\Newline source :start start)
        while (and end (< i line-number))
        finally (return (when (= i line-number)
                          (subseq source start end)))))
