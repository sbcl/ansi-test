;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Jan  6 05:41:06 2004
;;;; Contains: Tests of FILE-AUTHOR

(in-package :cl-test)

(deftest file-author.1
  (loop for pn in
	(directory (make-pathname :name :wild :type :wild
				  :defaults *default-pathname-defaults*))
	for author = (file-author pn)
	unless (or (null author) (stringp author))
	collect (list pn author))
  nil)

(deftest file-author.2
  (let ((author (file-author "file-author.lsp")))
    (if (or (null author) (stringp author))
	nil
      author))
  nil)

(deftest file-author.3
  (let ((author (file-author #p"file-author.lsp")))
    (if (or (null author) (stringp author))
	nil
      author))
  nil)

(deftest file-author.4
  (let ((author (file-author (truename "file-author.lsp"))))
    (if (or (null author) (stringp author))
	nil
      author))
  nil)

(deftest file-author.5
  (let ((author (with-open-file (s "file-author.lsp" :direction :input)
				(file-author s))))
    (if (or (null author) (stringp author))
	nil
      author))
  nil)

(deftest file-author.6
  (let ((author (let ((s (open "file-author.lsp" :direction :input)))
		  (close s)
		  (file-author s))))
    (if (or (null author) (stringp author))
	nil
      author))
  nil)

;;; Error tests

(deftest file-author.error.1
  (classify-error (file-author))
  program-error)

(deftest file-author.error.2
  (classify-error (file-author "file-author.lsp" nil))
  program-error)

(deftest file-author.error.3
  (classify-error
   (file-author (make-pathname :name :wild :type "lsp"
			       :defaults *default-pathname-defaults*)))
  file-error)

(deftest file-author.error.4
  (classify-error
   (file-author (make-pathname :name "file-author" :type :wild
			       :defaults *default-pathname-defaults*)))
  file-error)















