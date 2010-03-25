
(in-package #:cl-vectorizer)

;;------------------------------------------------------------------------------
;;  skeletonization
;;------------------------------------------------------------------------------
(defun A-condition (points)
  "A condition of Zong Suen operation"
  (let ((a 0) ) 
    (dotimes (i 8)
      (if (eq 1 (- (nth (+ i 1) points) (nth i points)))
	  (setq a (+ a 1))))
    a))


(defun B-condition (points)
  "B condition of Zong Suen operation"
  (apply '+ points))



(defun zong-suen-condition (point hash-points order)
  (let* ((p1 (get-hash-point-value point hash-points))
	 (p2 (get-hash-point-value (list (- (first point) 1) (second point)) hash-points))
	 (p3 (get-hash-point-value (list (- (first point) 1) (+ (second point) 1)) hash-points))
	 (p4 (get-hash-point-value (list (first point) (+ (second point) 1)) hash-points))
	 (p5 (get-hash-point-value (list (+ (first point) 1) (+ (second point) 1)) hash-points))
	 (p6 (get-hash-point-value (list (+ (first point) 1) (second point)) hash-points))
	 (p7 (get-hash-point-value (list (+ (first point) 1) (- (second point) 1)) hash-points))
	 (p8 (get-hash-point-value (list (first point) (- (second point) 1)) hash-points))
	 (p9 (get-hash-point-value (list (- (first point) 1) (- (second point) 1)) hash-points))
 	 (a-cond (A-condition (list p2 p3 p4 p5 p6 p7 p8 p9 p2)))
 	 (b-cond (B-condition (list p1 p2 p3 p4 p5 p6 p7 p8 p9)))
	 )
    (and 
     (< 2 b-cond) 
     (<= b-cond 6)
     (eq a-cond 1)
     (cond
       ((eq order 'first) (and 
			   (eq 0 (apply '* (list p2 p4 p6)))
			   (eq 0 (apply '* (list p4 p6 p8)))))
       ((eq order 'second) (and
			    (eq 0 (apply '* (list p2 p4 p8)))
			    (eq 0 (apply '* (list p2 p6 p8)))))))))

(defun zong-suen-first-condition (point hash-points)
  (zong-suen-condition point hash-points 'first))

(defun zong-suen-second-condition (point hash-points)
  (zong-suen-condition point hash-points 'second))

(defun thin-image-hash (hash-points)
  (let ((deleted 0)
	(deleted1 0)
	(deleted2 0)
	(ht (make-hash-table :test 'equal :size (round (* (hash-table-size hash-points) .7)))))
    (loop for point being the hash-key of hash-points do
	 (if (zong-suen-first-condition point hash-points)
	       (setf (gethash point ht) +black+)))
    (loop for point-to-remove being the hash-key of ht do
	 (remhash point-to-remove hash-points))
    (setf deleted (hash-table-count ht))
    (clrhash ht)
    (format t "first iteration. deleted ~d points ~%" deleted)
    (loop for point being the hash-key of hash-points do
	 (if (zong-suen-second-condition point hash-points)
	     (progn 
	       (setf (gethash point ht) +black+)
	       (incf deleted2 ))))
    
    (loop for point-to-remove being the hash-key of ht do
	 (remhash point-to-remove hash-points))
    
    (setf deleted (+ deleted (hash-table-count ht)))
    (format t "second iteration. deleted ~d points ~%" (hash-table-count ht))
    (clrhash ht)


    (if (< 0 (+ deleted1 deleted2))
	(progn
	  (format t "thinning more ... ~%------------------------------------------------------------~%"  )
	 (thin-image-hash hash-points))
	(progn
	  (setf deleted 1)
	  (loop while (< 0 deleted) do
	       (progn
		 (setf deleted 0)
		 (format t "-------------new delete -------------~%")
		 (loop for point being the hash-key of hash-points do
		      (when (should-delete-point point hash-points)
			(progn 
			  (incf deleted)
			  (remhash point hash-points))))
		 (format t "~a points deleted~%" deleted)))
	  hash-points))))
 
