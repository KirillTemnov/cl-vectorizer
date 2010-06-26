
(in-package #:cl-vectorizer)

;; Hough transformation module
;; using for search circles

(defun get-near-points (point points-list max-distance)
  "Get list of points that are at a `max-distance` or closer to `point`."
  (let (near-points)
    (dolist (pt points-list)
	 (let ((distance-to-pt (get-points-distance point pt)))
	   (when (and 
		  (>= max-distance distance-to-pt)
		  (< 0 distance-to-pt))
	     (push pt near-points)))
       near-points)))
	      
(defun get-circle-radius-and-center (p1 p2 p3 max-distance)
  "Get radius and coordinates of center point of circle, that build on points `p1`, `p2` and `p3`. If radius > `max-distance` / 2 than nil will returned, else return list (radius (cx cy))."
  ;;
  ;;
  ;;     1 (x2^2 - x3^2 + y2^2 - y3^2)(y1 - y2) - (x1^2 - x2^2 + y1^2 - y2^2)(y2 - y3)
  ;; a = - ---------------------------------------------------------------------------
  ;;     2                   (x2 - x3)(y1 - y2) - (x1 - x2)(y2 - y3)
  ;;
  ;;     1 (x1^2 - x2^2 + y1^2 - y2^2)(x2 - x3) - (x2^2 - x3^2 + y2^2 - y3^2)(x1 - x2)
  ;; b = - ---------------------------------------------------------------------------
  ;;     2                   (x2 - x3)(y1 - y2) - (x1 - x2)(y2 - y3)
  ;;
  ;;       _________________________
  ;; R = \/ (x1 - a)^2 + (y1 - b)^2
  ;;
  ;;  =>>>
  ;;
  ;;     1 (summand1)(y1 - y2) - (summand2)(y2 - y3)
  ;; a = - -----------------------------------------
  ;;     2                  divisor
  ;;
  ;;     1 (summand2)(x2 - x3) - (summand1)(x1 - x2)
  ;; b = - -----------------------------------------
  ;;     2                  divisor

  (let* ((x1 (first p1))
	 (x2 (first p2))
	 (x3 (first p3))
	 (y1 (second p1))
	 (y2 (second p2))
	 (y3 (second p3))
	 (divisor (-
		   (* (- x2 x3) (- y1 y2))
		   (* (- x1 x2) (- y2 y3))))

	 (summand1 (- 
		    (+ (expt x2 2) (expt y2 2))
		    (expt x3 2)
		    (expt y3 2)))

	 (summand2 (-
		    (+ (expt x1 2) (expt y1 2))
		    (expt x2 2)
		    (expt y2 2)))
	 (a (/
	     (-
	      (* summand1 (- y1 y2))
	      (* summand2 (- y2 y3)))
	     (* 2 divisor)))

	 (b (/
	     (-
	      (* summand2 (- x2 x3))	      
	      (* summand1 (- x1 x2)))
	     (* 2 divisor)))
	 (radius (sqrt (+
			(expt (- x1 a) 2)
			(expt (- y1 b) 2)))))
    (if (> (max (get-points-distance p1 p2) (get-points-distance p2 p3) (get-points-distance p3 p1) max-distance))
	nil
	(list radius (list a b)))))

(defun find-circles (hash-points max-distance)
  "Find circles by Hough transformation method."
  (let ((circles-hash (make-hash-table :test 'equal))
	(points-hash (hashtable-keys-to-list hash-points))
	circle-params
	(i 0))
    (dolist (p1 points-hash)
      (incf i)
      (format t "tick ~a ...~%" i)
      (dolist (p2 points-hash)
	(dolist (p3 points-hash)
	  (when (and
		 (not (equal p1 p2))
		 (not (equal p2 p3))
		 (not (equal p3 p1)))
	    (setf circle-params (get-circle-radius-and-center p1 p2 p3 max-distance))
	    (when circle-params
	      (format t "Circle params: ~a~%" circle-params)
	      (let ((hash-val (gethash circle-params circles-hash)))
		(if hash-val
		    (push (list p1 p2 p3) hash-val)
		    (setf hash-val (list (list p1 p2 p3))))
		(setf (gethash circle-params circles-hash) hash-val)))))))
    (if circle-params 
	(print-hash circle-params)
	(format t "Circle params are empty~%"  )))
;;    (format t "~%~%Points: ~a" (hashtable-keys-to-list points-hash))))
;;    (loop for point being the hash-key of hash-key do
	 


