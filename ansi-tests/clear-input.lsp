;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Wed Jan 28 06:12:39 2004
;;;; Contains: Tests of CLEAR-INPUT

(in-package :cl-test)

;;; These tests are limited, since whether an input stream can be
;;; cleared is not well specified.

(deftest clear-input.1
  (loop for s in (list *debug-io* *query-io*
		       *standard-input* *terminal-io*)
	always (eq (clear-input s) nil))
  t)

(deftest clear-input.2
  (clear-input)
  nil)

(deftest clear-input.3
  (clear-input nil)
  nil)

(deftest clear-input.4
  (clear-input t)
  nil)

;;; Error cases

(deftest clear-input.error.1
  :notes (:assume-no-simple-streams)
  (signals-error (clear-input t nil) program-error)
  t)

(deftest clear-input.error.2
  :notes (:assume-no-simple-streams)
  (signals-error (clear-input nil nil) program-error)
  t)

(deftest clear-input.error.3
  (signals-error (clear-input t nil nil) program-error)
  t)

(deftest clear-input.error.4
  (signals-error (clear-input nil nil nil) program-error)
  t)

(deftest clear-input.error.5
  (loop for x in *mini-universe*
	unless (or (member x '(nil t))
		   (typep x 'stream)
		   (equalt
		    (eval `(multiple-value-list
			    (signals-error (clear-input ',x) type-error)))
		    '(t)))
	collect x)
  nil)

			