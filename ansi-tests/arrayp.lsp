;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Jan 21 22:08:21 2003
;;;; Contains: Tests of ARRAYP

(in-package :cl-test)

;;; Also tested by make-array.lsp

(deftest arrayp.1
  (notnot (arrayp #(a b c)))
  t)

(deftest arrayp.2
  (notnot (arrayp "abcd"))
  t)

(deftest arrayp.3
  (notnot (arrayp #*001110101))
  t)

(deftest arrayp.4
  (notnot (arrayp #0aNIL))
  t)

(deftest arrayp.5
  (notnot (arrayp #2a((1 2 3)(4 5 6))))
  t)

(deftest arrayp.6
  (loop for e in *universe*
	for a = (arrayp e)
	for b = (typep e 'array)
	when (or (and a (not b))
		 (and b (not a)))
	collect e)
  nil)

;;; Error tests

(deftest arrayp.error.1
  (classify-error (arrayp))
  program-error)

(deftest arrayp.error.2
  (classify-error (arrayp #(a b c) nil))
  program-error)



