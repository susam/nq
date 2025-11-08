;;;; Nerd Quiz
;;;; =========

;;;; Copyright (c) 2025 Susam Pal
;;;;
;;;; You can use, copy, modify, merge, publish, distribute, sublicence
;;;; and/or sell copies of it, under the terms of the MIT Licence.
;;;; See LICENSE.md for complete details.
;;;;
;;;; This software is provided "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;; express or implied.  See LICENSE.md for complete details.


(require "uiop")

;;; Special Modes
;;; -------------

(defvar *main-mode* t
  "Run main function iff true.")


;;; General Definitions
;;; -------------------

(defun string-strip (string)
  "Remove spaces and newlines from both ends of the string."
  (string-trim '(#\Space #\Tab #\Newline) string))

(defun string-replace (old new string)
  "Replace old substring in string with new substring."
  (with-output-to-string (s)
    (let* ((next-index 0)
           (match-index))
      (loop
        (setf match-index (search old string :start2 next-index))
        (unless match-index
          (format s "~a" (subseq string next-index))
          (return))
        (format s "~a~a" (subseq string next-index match-index) new)
        (cond ((zerop (length old))
               (when (= next-index (length string))
                 (return))
               (format s "~a" (char string next-index))
               (incf next-index))
              (t
               (setf next-index (+ match-index (length old)))))))))

(defun string-split (string separator &key ignore-empty)
  "Split a string into a list of strings using the given separator."
  (let ((next-index 0)
        (match-index)
        (split)
        (result))
    (loop
      (setf match-index (search separator string :start2 next-index))
      (when (or (not match-index))
        (return))
      (setf split (subseq string next-index match-index))
      (unless (and (= (length split) 0) ignore-empty)
        (push split result))
      (setf next-index (+ match-index (length separator))))
    (when (< next-index (length string))
      (setf split (subseq string next-index match-index))
      (unless (and (= (length split) 0) ignore-empty)
        (push split result)))
    (reverse result)))

(defun fstr (fmt &rest args)
  "Format string using specified format and arguments."
  (apply #'format nil fmt args))

(defun read-file (filename)
  "Read file and close the file."
  (uiop:read-file-string filename))

(defun write-file (filename text)
  "Write text to file and close the file."
  (ensure-directories-exist filename)
  (with-open-file (f filename :direction :output :if-exists :supersede)
    (write-sequence text f)))


;;; JS Generator
;;; ------------

(defun indent (level)
  (make-string (* 2 level) :initial-element #\Space))

(defun alist-p (lst)
  (every (lambda (el) (and (consp el) (not (consp (car el))))) lst))

(defun to-js-string (s)
  (setf s (string-replace "\\" "\\\\" s))
  (setf s (string-replace (fstr "~%") "\\n" s))
  (setf s (string-replace "'" "\\'" s))
  (fstr "'~a'" s))

(defun to-js-key (key)
  (etypecase key
    (symbol (string-downcase (symbol-name key)))
    (string key)
    (number (princ-to-string key))))

(defun to-js-object (alist level)
  (with-output-to-string (out)
    (format out "{~%")
    (loop for (key . val) in alist
          for i from 1
          do (format out "~a~a: ~a"
                     (indent (1+ level))
                     (to-js-key key)
                     (to-js val (1+ level)))
             (when (< i (length alist))
               (format out ","))
             (format out "~%"))
    (format out "~a}" (indent level))))

(defun to-js-array (lst level)
  (with-output-to-string (out)
    (format out "[~%" (indent level))
    (loop for item in lst
          for i from 1
          do (format out "~a~a"
                     (indent (1+ level))
                     (to-js item (1+ level)))
             (when (< i (length lst))
               (format out ","))
             (format out "~%"))
    (format out "~a]" (indent level))))

(defun to-js (data &optional (level 0))
  (etypecase data
    (null (fstr "null" (indent level)))
    (number (princ-to-string data))
    (string (to-js-string data))
    (list
     (if (alist-p data)
         (to-js-object data level)
         (to-js-array data level)))))


;;; Tool Definitions
;;; ----------------

(defun read-card (filename)
  "Read one quiz card."
  (let* ((parts (string-split (read-file filename) (fstr "~%~%---~%~%")))
         (headers (string-split (nth 0 parts) (fstr "~%"))))
    (list (cons "date" (nth 0 headers))
          (cons "title" (nth 1 headers))
          (cons "tags" (string-split (nth 2 headers) ", "))
          (cons "authors" (string-split (nth 3 headers) ", "))
          (cons "question" (nth 1 parts))
          (cons "options" (subseq parts 2 (1- (length parts))))
          (cons "explanation" (string-strip (nth (1- (length parts)) parts))))))

(defun read-cards ()
  "Read all quiz cards."
  (loop for path in (directory "q/*.txt")
        collect (read-card path)))

(defun insert-cards (html cards)
  "Insert JS for cards into the given HTML."
  (let* ((start-token "const CARDS = ")
         (start-index (+ (search start-token html) (length start-token)))
         (end-index (search (fstr "~%    </script>") html :start2 start-index)))
    (fstr "~a~a~a" (subseq html 0 start-index) cards (subseq html end-index))))

(defun main ()
  "Generate quiz HTML."
  (let ((cards (to-js (read-cards) 3))
        (html (read-file "nq.html")))
    (setf html (insert-cards html cards))
    (write-file "nq.html" html)))

(when *main-mode*
  (main))
