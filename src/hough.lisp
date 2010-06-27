
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
	  (let* ((summand1 (- 
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
	    (if (< (max (get-points-distance p1 p2) (get-points-distance p2 p3) (get-points-distance p3 p1) (* 2 radius)) max-distance)
		nil
		(list radius (list (round a) (round b))))))))

(defun merge-two-circles (circle1 circle2)
  "Merge two circles in one. Circle here is a list: (radius (center-x center-y)).
Returns average circle (average center and average radius)."
  (list (avg (first circle1) (first circle2))
  	(list (round (avg (first (second circle1)) (first (second circle2))))
  	      (round (avg (second (second circle1)) (second (second circle2)))))))	       

(defun find-similar-circles (circle circles-hash)
  "Find circles, similar to `circle` by radius and center, inside `circles-hash`."  
  (flet ((similar-circles? (circ1 circ2)
	   ;; Check if `circ1` and `circ2` have approximately the same 
	   ;; radius and center point.
	   (let ((center-delta 5)       ; max center points distance is 4 points
		 (delta-r 2))			; radius may vary in 2 points
	     (and
	      (>= delta-r (- (first circ1) (first circ2)))
	      (>= center-delta (get-points-distance (second circ1) (second circ2)))))))

    (loop for cur-circle being the hash-key of circles-hash do
	 (when (and
		(not (equal circle cur-circle))
		(similar-circles? circle cur-circle))

	   (let* ((pts (gethash circle circles-hash))
		  (pts2 (gethash cur-circle circles-hash))
		  (new-pts (merge-lists-remove-duplicates pts pts2)))
	     (when (get-debug-mode)
	       (format t "Merge circles. ~%First: ~a~%" circle)
	       (format t "Second ~a ~%" cur-circle))

	     ;; remove both circles, add merged circle
	     (remhash circle circles-hash)
	     (remhash cur-circle circles-hash)
	     (setf (gethash (merge-two-circles circle cur-circle) circles-hash) new-pts))))))


(defun merge-hashed-circles (circles-hash)
  "Merge of several hashes circles in one.
As circles may have offsets during the vectorization process, inside `circles-hash`
may real cirlces consists of several circles with small offset of center point ( < 3 pt)
and small offset from circle radius. This method merge such circles *putting all points in one resulting circle*. Resulting circle and its points returns to `circles-hash`, other circles removed from it."
  (loop for circle being the hash-key of circles-hash do
       (find-similar-circles circle circles-hash)))

(defun find-circles (points-hash max-distance)
  "Find circles by Hough transformation method."
  (let ((circles-hash (make-hash-table :test 'equal))
	(points-list (hashtable-keys-to-list points-hash))
	circle-params
	near-points-list
	p1 (i 0))
    (loop while (< 0 (length points-list)) do
	 (progn
	   (incf i)
	   (when (get-debug-mode)
	     (format t "tick ~a. List length = ~a...~%" i (length points-list)))
	   (setf p1 (first points-list))
	   (setf points-list (cdr points-list))
	   (setf near-points-list (get-near-points p1 points-list max-distance))
	   (dolist (p2 near-points-list)
	     (dolist (p3 near-points-list)
	       (setf circle-params (get-circle-radius-and-center p1 p2 p3 max-distance))
	       (when circle-params
		 ;;	    (format t "Circle params: ~a~%" circle-params)
		 (let ((hash-val (gethash circle-params circles-hash)))
		   (setf hash-val (push-to-list-if-not-present hash-val p1 p2 p3))
		   (setf (gethash circle-params circles-hash) hash-val)))))
	   ;;    (format t "Circle params: ~a~%" circles-hash)))

	   (loop for circle-params being the hash-key of circles-hash do
		(let ((lst (gethash circle-params circles-hash)))
		  (when (> 5 (length lst))	; change this to more complex condition
		    (remhash circle-params circles-hash))))
	   (merge-hashed-circles circles-hash)))

    (loop for circle being the hash-key of circles-hash do
          (when (> 10 (length (gethash circle circles-hash)))
	    (remhash circle circles-hash)))
    circles-hash))

