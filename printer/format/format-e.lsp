;-*- Mode:     Lisp -*-
;;;; Author:   Marius Gerbershagen
;;;; Created:  Wed Feb  6 19:35:53 2019
;;;; Contains: Tests of the ~e format directive




;;; Equivalent to PRIN1 for (abs x) outside of the range [10^-3,10^7),
;;; apart from the sign of the exponent, which is always printed.

(deftest format.e.1
  (let ((*print-readably* nil)
        (fn (formatter "~e")))
    (loop
     for i = (random 4)
     for type = (elt #(short-float single-float double-float long-float) i)
     for min-value = (elt (vector least-positive-short-float least-positive-single-float
                                  least-positive-double-float least-positive-long-float)
                          i)
     for max-value = (elt (vector most-positive-short-float most-positive-single-float
                                  most-positive-double-float most-positive-long-float)
                          i)
     for x = (expt (coerce 10 type)
                   (if (= (random 2) 0)
                       (- -3 (random (- -3 (log min-value 10))))
                       (+ 7 (random (- (log max-value 10) 7)))))
     for s1 = (let ((*read-default-float-format* type)) (format nil "~e" x))
     for s2 = (let* ((*read-default-float-format* type)
                     (s (prin1-to-string x))
                     (exp-pos (1+ (position #\e s))))
                (if (> x 1)
                    (concatenate 'string (subseq s 0 exp-pos) "+" (subseq s exp-pos))
                    s))
     for s3 = (let ((*read-default-float-format* type))
                (formatter-call-to-string fn x))
     repeat 1000
     when (and (or (< x 1/1000)
                   (>= x 10000000))
               (or (not (string= s1 s2))
                   (not (string= s1 s3))))
     collect (list x s1 s2 s3)))
  nil)

(deftest format.e.2
  (let ((*print-readably* nil)
        (fn (formatter "~e")))
    (loop
     for i = (random 4)
     for type = (elt #(short-float single-float double-float long-float) i)
     for min-value = (elt (vector least-negative-short-float least-negative-single-float
                                  least-negative-double-float least-negative-long-float)
                          i)
     for max-value = (elt (vector most-negative-short-float most-negative-single-float
                                  most-negative-double-float most-negative-long-float)
                          i)
     for x = (- (expt (coerce 10 type)
                      (if (= (random 2) 0)
                          (- -3 (random (- -3 (log (abs min-value) 10))))
                          (+ 7 (random (- (log (abs max-value) 10) 7))))))
     for s1 = (let ((*read-default-float-format* type)) (format nil "~e" x))
     for s2 = (let* ((*read-default-float-format* type)
                     (s (prin1-to-string x))
                     (exp-pos (1+ (position #\e s))))
                (if (< x -1)
                    (concatenate 'string (subseq s 0 exp-pos) "+" (subseq s exp-pos))
                    s))
     for s3 = (let ((*read-default-float-format* type))
                (formatter-call-to-string fn x))
     repeat 1000
     when (and (or (> x -1/1000)
                   (<= x -10000000))
               (or (not (string= s1 s2))
                   (not (string= s1 s3))))
     collect (list x s1 s2 s3)))
  nil)



;;; If the parameter d is omitted, [...] a value is chosen for d in
;;; such a way that as many digits as possible may be printed subject
;;; to [...] the constraint that no trailing zero digits may appear in
;;; the fraction, except that if the fraction to be printed is zero
;;; then a single zero digit should appear after the decimal point.

(deftest format.e.3
  (let ((fn (formatter "~6e")))
    (loop for x in '(0 0.0s0 0.0f0 0.0d0 0.0l0)
          for type in '(single-float short-float single-float
                        double-float long-float)
          for s = (let ((*read-default-float-format* type))
                    (format nil "~6e" x))
          for s2 = (let ((*read-default-float-format* type))
                     (formatter-call-to-string fn x))
          unless (and (string= s "0.0e+0") (string= s s2))
          collect (list x s s2)))
  nil)

(deftest format.e.4
  (let ((fn (formatter "~5e")))
    (loop for x in '(1 1.0s0 1.0f0 1.0d0 1.0l0)
          for type in '(single-float short-float single-float
                        double-float long-float)
          for s = (let ((*read-default-float-format* type))
                    (format nil "~5e" x))
          for s2 = (let ((*read-default-float-format* type))
                     (formatter-call-to-string fn x))
          unless (and (string= s "1.e+0") (string= s s2))
          collect (list x s s2)))
  nil)

(deftest format.e.5
  (let ((fn (formatter "~4e")))
    (loop for x in '(1 1.0s0 1.0f0 1.0d0 1.0l0)
          for type in '(single-float short-float single-float
                        double-float long-float)
          for s = (let ((*read-default-float-format* type))
                    (format nil "~4e" x))
          for s2 = (let ((*read-default-float-format* type))
                     (formatter-call-to-string fn x))
          unless (and (string= s "1.e+0") (string= s s2))
          collect (list x s s2)))
  nil)

(deftest format.e.6
  (let ((fn (formatter "~6e")))
    (loop for x in '(1 1.0s0 1.0f0 1.0d0 1.0l0)
          for type in '(single-float short-float single-float
                        double-float long-float)
          for s = (let ((*read-default-float-format* type))
                    (format nil "~6e" x))
          for s2 = (let ((*read-default-float-format* type))
                     (formatter-call-to-string fn x))
          unless (and (string= s " 1.e+0") (string= s s2))
          collect (list x s s2)))
  nil)

(deftest format.e.7
  (let ((fn (formatter "~6@e")))
    (loop for x in '(1 1.0s0 1.0f0 1.0d0 1.0l0)
          for type in '(single-float short-float single-float
                        double-float long-float)
          for s = (let ((*read-default-float-format* type))
                    (format nil "~6@e" x))
          for s2 = (let ((*read-default-float-format* type))
                     (formatter-call-to-string fn x))
          unless (and (string= s "+1.e+0") (string= s s2))
          collect (list x s s2)))
  nil)

(deftest format.e.8
  (let ((fn (formatter "~5@e")))
    (loop for x in '(1 1.0s0 1.0f0 1.0d0 1.0l0)
          for type in '(single-float short-float single-float
                        double-float long-float)
          for s = (let ((*read-default-float-format* type))
                    (format nil "~5@e" x))
          for s2 = (let ((*read-default-float-format* type))
                     (formatter-call-to-string fn x))
          unless (and (string= s "+1.e+0") (string= s s2))
          collect (list x s s2)))
  nil)

(deftest format.e.9
  (let ((fn (formatter "~6e")))
    (loop for x in '(1 1.0s0 1.0f0 1.0d0 1.0l0)
          for type in '(single-float short-float single-float
                        double-float long-float)
          for s = (let ((*read-default-float-format* type))
                    (format nil "~6e" (- x)))
          for s2 = (let ((*read-default-float-format* type))
                     (formatter-call-to-string fn (- x)))
          unless (and (string= s "-1.e+0") (string= s s2))
          collect (list x s s2)))
  nil)


;;; d parameter

(deftest format.e.10
  (let ((fn (formatter "~7,2e")))
    (loop for x in '(1/2 0.5s0 0.5f0 0.5d0 0.5l0)
          for type in '(single-float short-float single-float
                        double-float long-float)
          for s = (let ((*read-default-float-format* type))
                    (format nil "~7,2e" x))
          for s2 = (let ((*read-default-float-format* type))
                     (formatter-call-to-string fn x))
          unless (and (string= s "5.00e-1") (string= s s2))
          collect (list x s s2)))
  nil)

(deftest format.e.11
  (let ((fn (formatter "~6,2e")))
    (loop for x in '(1/2 0.5s0 0.5f0 0.5d0 0.5l0)
          for type in '(single-float short-float single-float
                        double-float long-float)
          for s = (let ((*read-default-float-format* type))
                    (format nil "~6,2e" x))
          for s2 = (let ((*read-default-float-format* type))
                     (formatter-call-to-string fn x))
          unless (and (string= s "5.00e-1") (string= s s2))
          collect (list x s s2)))
  nil)

(deftest format.e.12
  (let ((fn (formatter "~7,2@e")))
    (loop for x in '(1/2 0.5s0 0.5f0 0.5d0 0.5l0)
          for type in '(single-float short-float single-float
                        double-float long-float)
          for s = (let ((*read-default-float-format* type))
                    (format nil "~7,2@e" x))
          for s2 = (let ((*read-default-float-format* type))
                     (formatter-call-to-string fn x))
          unless (and (string= s "+5.00e-1") (string= s s2))
          collect (list x s s2)))
  nil)

(deftest format.e.13
  (let ((fn (formatter "~,2e")))
    (loop for x in '(1/2 0.5s0 0.5f0 0.5d0 0.5l0)
          for type in '(single-float short-float single-float
                        double-float long-float)
          for s = (let ((*read-default-float-format* type))
                    (format nil "~,2e" x))
          for s2 = (let ((*read-default-float-format* type))
                     (formatter-call-to-string fn x))
          unless (and (string= s "5.00e-1") (string= s s2))
          collect (list x s s2)))
  nil)

(deftest format.e.14
  (let ((fn (formatter "~,2e")))
    (loop for x in '(1/2 0.5s0 0.5f0 0.5d0 0.5l0)
          for type in '(single-float short-float single-float
                        double-float long-float)
          for s = (let ((*read-default-float-format* type))
                    (format nil "~,2e" (- x)))
          for s2 = (let ((*read-default-float-format* type))
                     (formatter-call-to-string fn (- x)))
          unless (and (string= s "-5.00e-1") (string= s s2))
          collect (list x s s2)))
  nil)


;;; e parameter

(deftest format.e.15
  (let ((fn (formatter "~,2,2e")))
    (loop for x in '(1/2 0.5s0 0.5f0 0.5d0 0.5l0)
          for type in '(single-float short-float single-float
                        double-float long-float)
          for s = (let ((*read-default-float-format* type))
                    (format nil "~,2,2e" x))
          for s2 = (let ((*read-default-float-format* type))
                     (formatter-call-to-string fn x))
          unless (and (string= s "5.00e-01") (string= s s2))
          collect (list x s s2)))
  nil)


;;; k parameter

(deftest format.e.16
  (let ((fn (formatter "~,2,,-1e")))
    (loop for x in '(5 5.0s0 5.0f0 5.0d0 5.0l0)
          for type in '(single-float short-float single-float
                        double-float long-float)
          for s = (let ((*read-default-float-format* type))
                    (format nil "~,2,,-1e" x))
          for s2 = (let ((*read-default-float-format* type))
                     (formatter-call-to-string fn x))
          unless (and (string= s "0.05e+2") (string= s s2))
          collect (list x s s2)))
  nil)

(deftest format.e.17
  (let ((fn (formatter "~,2,,0e")))
    (loop for x in '(1/2 0.5s0 0.5f0 0.5d0 0.5l0)
          for type in '(single-float short-float single-float
                        double-float long-float)
          for s = (let ((*read-default-float-format* type))
                    (format nil "~,2,,0e" x))
          for s2 = (let ((*read-default-float-format* type))
                     (formatter-call-to-string fn x))
          unless (and (string= s "0.50e+0") (string= s s2))
          collect (list x s s2)))
  nil)

(deftest format.e.18
  (let ((fn (formatter "~,2,,2e")))
    (loop for x in '(1/20 0.05s0 0.05f0 0.05d0 0.05l0)
          for type in '(single-float short-float single-float
                        double-float long-float)
          for s = (let ((*read-default-float-format* type))
                    (format nil "~,2,,2e" x))
          for s2 = (let ((*read-default-float-format* type))
                     (formatter-call-to-string fn x))
          unless (and (string= s "50.0e-3") (string= s s2))
          collect (list x s s2)))
  nil)

(deftest format.e.19
  (let ((fn (formatter "~,2,,3e")))
    (loop for x in '(1/20 0.05s0 0.05f0 0.05d0 0.05l0)
          for type in '(single-float short-float single-float
                        double-float long-float)
          for s = (let ((*read-default-float-format* type))
                    (format nil "~,2,,3e" x))
          for s2 = (let ((*read-default-float-format* type))
                     (formatter-call-to-string fn x))
          unless (and (string= s "500.e-4") (string= s s2))
          collect (list x s s2)))
  nil)

(deftest format.e.20
  (let ((fn (formatter "~,2,,4e")))
    (loop for x in '(1/20 0.05s0 0.05f0 0.05d0 0.05l0)
          for type in '(single-float short-float single-float
                        double-float long-float)
          for s = (let ((*read-default-float-format* type))
                    (format nil "~,2,,4e" x))
          for s2 = (let ((*read-default-float-format* type))
                     (formatter-call-to-string fn x))
          unless (and (string= s "5000.e-5") (string= s s2))
          collect (list x s s2)))
  nil)


;;; overflow

(deftest format.e.21
  (let ((fn (formatter "~6,2,,,'*e")))
    (loop for x in '(1/2 0.5s0 0.5f0 0.5d0 0.5l0)
          for type in '(single-float short-float single-float
                        double-float long-float)
          for s = (let ((*read-default-float-format* type))
                    (format nil "~6,2,,,'*e" x))
          for s2 = (let ((*read-default-float-format* type))
                     (formatter-call-to-string fn x))
          unless (and (string= s "******") (string= s s2))
          collect (list x s s2)))
  nil)

(deftest format.e.22
  (let ((fn (formatter "~7,2,,,'*e")))
    (loop for x in '(1/2 0.5s0 0.5f0 0.5d0 0.5l0)
          for type in '(single-float short-float single-float
                        double-float long-float)
          for s = (let ((*read-default-float-format* type))
                    (format nil "~7,2,,,'*e" x))
          for s2 = (let ((*read-default-float-format* type))
                     (formatter-call-to-string fn x))
          unless (and (string= s "5.00e-1") (string= s s2))
          collect (list x s s2)))
  nil)

;;; padchar

(deftest format.e.23
  (let ((fn (formatter "~10,1,,,e")))
    (loop for x in '(1/2 0.5s0 0.5f0 0.5d0 0.5l0)
          for type in '(single-float short-float single-float
                        double-float long-float)
          for s = (let ((*read-default-float-format* type))
                    (format nil "~10,1,,,e" x))
          for s2 = (let ((*read-default-float-format* type))
                     (formatter-call-to-string fn x))
          unless (and (string= s "    5.0e-1") (string= s s2))
          collect (list x s s2)))
  nil)

(deftest format.e.24
  (let ((fn (formatter "~10,1,,,,'*e")))
    (loop for x in '(1/2 0.5s0 0.5f0 0.5d0 0.5l0)
          for type in '(single-float short-float single-float
                        double-float long-float)
          for s = (let ((*read-default-float-format* type))
                    (format nil "~10,1,,,,'*e" x))
          for s2 = (let ((*read-default-float-format* type))
                     (formatter-call-to-string fn x))
          unless (and (string= s "****5.0e-1") (string= s s2))
          collect (list x s s2)))
  nil)

;;; exponentchar

(deftest format.e.25
  (let ((fn (formatter "~,2,,,,,'^e")))
    (loop for x in '(1/2 0.5s0 0.5f0 0.5d0 0.5l0)
          for type in '(single-float short-float single-float
                        double-float long-float)
          for s = (let ((*read-default-float-format* type))
                    (format nil "~,2,,,,,'^e" x))
          for s2 = (let ((*read-default-float-format* type))
                     (formatter-call-to-string fn x))
          unless (and (string= s "5.00^-1") (string= s s2))
          collect (list x s s2)))
  nil)
