;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Aug  9 08:08:00 2003
;;;; Contains: Aux. functions used in FLOOR tests



(defun floor.1-fn ()
  (loop for n = (- (random 2000000000)
                   1000000000)
        for d = (1+ (random 10000))
        for vals = (multiple-value-list (floor n d))
        for (q r) = vals
        for n2 = (+ (* q d) r)
        repeat 1000
        unless (and (eql (length vals) 2)
                    (integerp q)
                    (= n n2)
                    (integerp r)
                    (< -1 r d))
        collect (list n d q r n2)))

(defun floor.2-fn ()
  (loop for num = (random 1000000000)
        for denom = (1+ (random 1000))
        for n = (/ num denom)
        for d = (1+ (random 10000))
        for vals = (multiple-value-list (floor n d))
        for (q r) = vals
        for n2 = (+ (* q d) r)
        repeat 1000
        unless (and (eql (length vals) 2)
                    (integerp q)
                    (<= 0 r)
                    (< r d)
                    (= n n2))
        collect (list n d q r n2)))

(defun floor.3-fn (width)
  (loop for n = (- (random width) (/ width 2))
        for vals = (multiple-value-list (floor n))
        for (q r) = vals
        for n2 = (+ q r)
        repeat 1000
        unless (and (eql (length vals) 2)
                    (integerp q)
                    (= n n2)
                    (<= 0 r)
                    (< r 1)
                    )
        collect (list n q r n2)))

(defun floor.7-fn ()
  (loop for numerator = (- (random 10000000000) 5000000000)
        for denominator = (1+ (random 100000))
        for n = (/ numerator denominator)
        for vals = (multiple-value-list (floor n))
        for (q r) = vals
        for n2 = (+ q r)
        repeat 1000
        unless (and (eql (length vals) 2)
                    (integerp q)
                    (rationalp r)
                    (= n n2)
                    (<= 0 r)
                    (< r 1)
                    )
        collect (list n q r n2)))

(defun floor.8-fn ()
  (loop for num1 = (- (random 10000000000) 5000000000)
        for den1 = (1+ (random 100000))
        for n = (/ num1 den1)
        for num2 = (- (1+ (random 1000000)))
        for den2 = (1+ (random 1000000))
        for d = (/ num2 den2)
        for vals = (multiple-value-list (floor n d))
        for (q r) = vals
        for n2 = (+ (* q d) r)
        repeat 1000
        unless (and (eql (length vals) 2)
                    (integerp q)
                    (rationalp r)
                    (>= 0 r)
                    (> r d)
                    (= n n2))
        collect (list n q d r n2)))

(defun floor.9-fn ()
  (loop for num1 = (- (random 1000000000000000) 500000000000000)
        for den1 = (1+ (random 10000000000))
        for n = (/ num1 den1)
        for num2 = (- (1+ (random 1000000000)))
        for den2 = (1+ (random 10000000))
        for d = (/ num2 den2)
        for vals = (multiple-value-list (floor n d))
        for (q r) = vals
        for n2 = (+ (* q d) r)
        repeat 1000
        unless (and (eql (length vals) 2)
                    (integerp q)
                    (rationalp r)
                    (>= 0 r)
                    (> r d)
                    (= n n2))
        collect (list n q d r n2)))

;;; Need float tests
