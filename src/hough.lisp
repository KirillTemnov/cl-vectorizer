
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
	     (push pt near-points))))
       near-points))
	      
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

;  (when (not (equal p2 p3))
    (let* ((x1 (first p1))
	   (x2 (first p2))
	   (x3 (first p3))
	   (y1 (second p1))
	   (y2 (second p2))
	   (y3 (second p3))
	   (divisor (-
		     (* (- x2 x3) (- y1 y2))
		     (* (- x1 x2) (- y2 y3)))))
      (if (= 0 divisor)
	  nil	  
	  (let* ((summand1 (-  ;;   (format t "divisor : ~a~%" divisor)
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
;;	    (list radius (list a b))))))
	    (if (< (max (get-points-distance p1 p2) (get-points-distance p2 p3) (get-points-distance p3 p1) (* 2 radius)) max-distance)
		nil
		(list radius (list a b)))))))

(defun find-circles (points-hash max-distance)
  "Find circles by Hough transformation method."
  (let ((circles-hash (make-hash-table :test 'equal))
	(points-list (hashtable-keys-to-list points-hash))
	circle-params
	near-points-list
	(i 0))
    (dolist (p1 points-list)
      (incf i)
      (format t "tick ~a ...~%" i)
      (setf near-points-list (get-near-points p1 points-list max-distance))
      (dolist (p2 near-points-list)
	(dolist (p3 near-points-list)
	  (setf circle-params (get-circle-radius-and-center p1 p2 p3 max-distance))
	  (when circle-params
;;	    (format t "Circle params: ~a~%" circle-params)
	    (let ((hash-val (gethash circle-params circles-hash)))
	      (setf hash-val (push-to-list-if-not-present hash-val p1 p2 p3))
	      ;; (if hash-val
	      ;; 	  (when (not (member (list p1 p2 p3) hash-val 
	      ;; 			     :test #'(lambda (l1 l2) 
	      ;; 				       (or
	      ;; 				       	(member (first l1) l2)
	      ;; 				       	(member (second l1) l2)
	      ;; 				       	(member (third l1) l2)))))
	      ;; 	    (progn
	      ;; 	      (push p1 hash-val)
	      ;; 	      (push p2 hash-val)
	      ;; 	      (push p3 hash-val))
	      ;; 	  (setf hash-val (list (list p1 p2 p3))))
;;	      (format t "Hash val : ~a~%" hash-val)
	      (setf (gethash circle-params circles-hash) hash-val))))))
;;    (format t "Circle params: ~a~%" circles-hash)))

    (loop for circle-params being the hash-key of circles-hash do
	 (let ((lst (gethash circle-params circles-hash)))
	    (when (> 18 (length lst))
	      (remhash circle-params circles-hash))))

    (format t "Hash size : ~a ~%" (hash-table-count circles-hash))
;;    ))
    (print-hash circles-hash)))


	 


