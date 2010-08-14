
(in-package #:cl-vectorizer)


;; testing suite
(defun qtree-test-suite nil
  "Test of correct finding pathes by functions BOTTOM-NEIB, LEFT-NEIB, TOP-NEIB, RIGHT-NEIB, GET-PATHES-LIST."
  (assert (equal (bottom-neib '(0 3 0)) '(2 3 0)))
  (assert (equal (top-neib '(0 3 0)) '(2 1 0)))
  (assert (equal (left-neib '(0 3 0)) '(1 2 0)))
  (assert (equal (right-neib '(0 3 0)) '(1 3 0)))
  (assert (equal (bottom-neib '(1 1 2)) '(3 1 2)))
  (assert (equal (top-neib '(1 1 2)) '(3 3 0)))
  (assert (equal (left-neib '(1 1 2)) '(0 1 2)))
  (assert (equal (right-neib '(1 1 2)) '(0 0 3)))
  (assert (equal (top-neib '(1 0 0 0 0)) '(3 2 2 2 2)))
  (assert (equal (get-pathes-list '(0 3 0)) '((1 2 0) (2 1 0) (2 3 0) (1 3 0))))
  (assert (equal (get-pathes-list '(1 1 2)) '((0 0 3) (3 3 0) (3 1 2) (0 1 2))))
  (format nil "ok"  ))

(defun qtree-test-offset nil
  "Test offset function"
  (assert (equal (offset '(0 0 0)) '(0 0)))
  (assert (equal (offset '(1 0 0)) '(1 0)))
  (assert (equal (offset '(2 0 0)) '(0 1)))
  (assert (equal (offset '(3 0 0)) '(1 1)))
  (assert (equal (offset '(2 1 0)) '(2 1)))
  (assert (equal (offset '(0 3 0)) '(2 2)))
  (assert (equal (offset '(1 0 2)) '(1 4)))
  (assert (equal (offset '(2 1 2)) '(2 5)))
  (assert (equal (offset '(1 2 1)) '(5 2)))
  (assert (equal (offset '(2 2 1)) '(4 3)))
  (assert (equal (offset '(3 2 1)) '(5 3)))
  (assert (equal (offset '(0 0 3)) '(4 4)))
  (format nil "ok"))

(defun vectorize-test-suite nil
  "Test helpers module"
  (assert (> 0.5 (abs (- 45  (ta2 '((10 10) (20 20)))))))
  (assert (> 0.5 (abs (- 135  (ta2 '((10 10) (0 20)))))))
  (assert (> 0.5 (abs (- 225 (ta2 '((10 10) (0 0)))))))
  (assert (> 0.5 (abs (- 315 (ta2 '((10 10) (20 0)))))))
  (format nil "ok"))



(qtree-test-offset)
(qtree-test-suite)

