
(in-package #:cl-vectorizer)

;; testing suite
(defun qtree-test-suite nil
  "Test of correct finding pathes by functions BOTTOM-NEIB, LEFT-NEIB, TOP-NEIB, RIGHT-NEIB."
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


(qtree-test-suite)

