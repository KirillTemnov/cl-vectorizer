
(in-package #:cl-vectorizer)

;; "
;; Что нужно сделать?
;; 1) Получить первую точку из массива точек у которой будет 1 сосед.
;;    Это будет начало новой линии. Удалить точку из хеша.
;; 2) Найти соседнюю точку и получить направление.
;;    Направление не может меняться более чем на 90 грудусов.
;;    Удалить точку из хеша.
;; 3) Если у соседней точки нет соседей или направление сильно меняется - добавить новую линию,
;;    иначе перейти к п 2)
;; Закончить когда больше не останется точек

;; Вспомогательные функции
;; - угол наклона прямой
;; "
;; DEBUG
(defun print-line (line)
  "Prints line for debug purposes.
   line is a list:  '((x1 y1) (x2 y2) length)"
  (when (not (line? line))
    (format t "~a is not a line!~%" line)
    (error "wrong line format!"))
  (format t "#Line {~a}  start:[~a]  end:[~a]  length:~a~%" line (first line) (second line) (third line)))

(defun line? (object)
  "Return T if object can be a line representation, otherwise returns nil."
  (cond
    ((atom object) nil)
    ((and
      (eq 3 (length object))
      (listp (first object))
      (listp (second object))
      (atom (third object))) t)
    (t nil)))

;; DEBUG
(defun get-line-string (line)
  "Returns line representation.
   line is a list:  '((x1 y1) (x2 y2) length)"
  (format nil "#Line {~a}  start:[~a]  end:[~a]  length:~a~%" line (first line) (second line) (third line)))

(defun get-points-distance (p1 p2)
  "Returns eucleadian distance between 2D points p1 and p2"
  (let* ((x1 (first p1))
	 (y1 (second p1))
	 (x2 (first p2))
	 (y2 (second p2))
	 (dx (- x2 x1))
	 (dy (- y2 y1)))
    (sqrt (+ (* dx dx)  (* dy dy)))))

(defun make-line (p1 p2)
  "Create line which starts from left (0) to right (infinity)"
  (cond
    ((< (first p1) (first p2)) (list p1 p2 (get-points-distance p1 p2)))
    (t (list p2 p1 (get-points-distance p1 p2)))))

(defun get-line-length (line)
  "Returns length of 2D line.
   Example:
   line = '((3 3) (11 4) length)
   will return 8.062258"
  (let* ((x1 (caar line))
	 (y1 (cadar line))
	 (x2 (caadr line))
	 (y2 (cadadr line))
	 (dx (- x2 x1))
	 (dy (- y2 y1)))
    (sqrt (+ (* dx dx)  (* dy dy)))))

(defun compare-points (x y)
  "Compare two points, point less if it plased from left and(or) above another point."
  (cond 
    ((eq (first x) (first y))
     (> (second x) (second y)))
    (t
     (< (first x) (first y)))))

;; (sort '((59 84) (59 58) (59 99) (59 87)) #'compare-points)
;;  (sort '((59 84) (60 58) (45 99) (58 87)) #'compare-points)

(defun get-tilt-angle (line)
  "Returns tilt angle between line and horizont. Result return in degrees."
  (let* ((p1 (first line))
	 (p2 (second line))
	 (dx (- (first p1) (first p2)))
	 (dy (- (second p1) (second p2))))
    (cond
      ((= 0 dx) 90)
      (t (rad-to-degree (atan (/ dy dx)))))))


    ;; (flet ((min-x-point (points-list)
    ;; 	     (let* ((min-pt (first points-list))
    ;; 		   (min-value (first min-pt)))
    ;; 	       (dolist (pt points-list)
    ;; 		 (when (> min-value (first pt))
    ;; 		   (setf min-value (first pt))
    ;; 		   (setf min-pt pt)))
    ;; 	       min-pt)))
    ;;   (flet ((max-x-poi
    ;; (setf result 
    ;; 	  (make-line (list 
    ;; 		      (min (caar line1) (caadr line1) (caar line2) (caadr line2))
    ;; 		      ( 
(defun merge-two-lines (line1 line2)
  "Merge two lines in one"
  (let*
      ((points (sort (list (first line1) (second line1) (first line2) (second line2)) #'compare-points))
       (start (first points))
       (end (fourth points))
      (result (make-line start end)))
	  ;; (cond	    
	  ;;   ((< (caar line1) (caar line2))
	  ;;    (make-line (first line1) (second line2)))
	  ;;   (t 
	  ;;    (make-line (first line2) (second line1)))))
    (when  (< (third result) (+ (third line1) (third line2)))
      (format t "Merge lines ~a and ~a~%" (get-line-string line1) (get-line-string line2))
      (format t "Result: ~a~%~%" result))
    result))

(defun substitute-angles (an1 an2)
  "Return absolute difference between two angles in degrees."
  (let ((result (abs (- an1 an2))))
    (cond
      ((> result 180) (- 360 result))
      (t result))))

(defun point-belong-to-line? (point start-line-point end-line-point tilt-angle)
  "Check if point could belong to line."
  (let ((distance (get-points-distance start-line-point end-line-point))
	 (new-tilt (get-tilt-angle (list start-line-point point))))
    (> +max-angle-on-line+ (substitute-angles tilt-angle new-tilt)))) ; 15 degrees 
    ;; (cond
    ;;   ((and
    ;; 	(> 1400 distance)
    ;; 	(> +max-angle-on-line+  (substitute-angles new-tilt tilt-angle))) t)
    ;;   (t nil))))
    ;;   ((and
    ;;   	(> 5 distance)
    ;;   	(> +max-angle-on-line+  (substitute-angles new-tilt tilt-angle))) t)
    ;;   ((and
    ;;   	(> 50 distance)
    ;;   	(> +min-angle-on-line+ (substitute-angles new-tilt tilt-angle)) t))
    ;;   ((= 0  (substitute-angles new-tilt tilt-angle)) t)
    ;;   (t nil))))
       
(defun find-end-of-line (point hash-points &key (start-point nil) (tilt-angle nil))
  "Searches end of line in hash-points."
  (let* ((active-points (get-neibhour-active-points point hash-points)))
    (remhash point hash-points)
    (cond 
      ((=  0 (length active-points))		; end of line
       (progn
	 (when (eq nil start-point) (error "Error - line from one point"))
	 (make-line start-point point)))

      ((eq nil start-point)		; begin of line
       (let ((cur-point nil))		; read next 2 points and make recursive call

	 (if  (>= 3 (length active-points))
	      nil				; if line shorter than 3 points return nil
	      (progn
		(dotimes (i 3)
		  (setf cur-point (first active-points))
		  (remhash cur-point hash-points)
		  (setf active-points (get-neibhour-active-points cur-point hash-points))
		  (when (= 0 (length active-points))
		    (return)))
		(if (= 0 (length active-points))
		    nil
		    (let* ((end (first active-points))
			   (start point)
			   (tilt-angle (get-tilt-angle (list start end))))
		      (find-end-of-line end hash-points :start-point point :tilt-angle tilt-angle)))))))

      (t				; read next point of line
       (let  ((end-point (first active-points)))
	      (if (point-belong-to-line? point start-point end-point tilt-angle)
		  (find-end-of-line end-point hash-points :start-point start-point :tilt-angle tilt-angle)
		  (progn
		    (make-line start-point end-point)
		    (setf (gethash point hash-points) 1))))))))

       ;; (let* ((end-point (first active-points))
       ;; 	      (new-tilt-angle (get-tilt-angle (list start-point end-point))))
       ;; 	 (if (> +max-angle-on-line+ (abs (- tilt-angle new-tilt-angle))) ; 15 degrees max
       ;; 	     (find-end-of-line end-point hash-points :start-point start-point :tilt-angle tilt-angle)
       ;; 	     (make-line start-point point )))))))

(defun point-have-one-neibhour? (point hash-points)
  "Returns T if point have only one neibhour."
  (= 1 (apply #'+ (get-neibhour-points point hash-points))))

(defun vectorize-hash (hash-points)
  "Vectorize hash with points and return hash, consists of lines (as keys)."
  (let ((hash-lines (make-hash-table :test 'equal ))
	(line nil) (hash-len (1+ (hash-table-count hash-points))))
    (loop while (and (> hash-len (hash-table-count hash-points)) (< 0 (hash-table-count hash-points))) do
	 (setf hash-len (hash-table-count hash-points))

	 (loop for point being the hash-key of hash-points do
	      (when (point-have-one-neibhour? point hash-points)
		(progn
		  (setf line (find-end-of-line point hash-points))
		  (when (and (not (line? line)) (get-debug-mode))
		    (format t "point = ~a    line = ~a~%" point line))
		  (when (line? line)
		    (when (get-debug-mode) (format t "add line ") (print-line line))
		    ;; (progn
		    ;;(print-line line)
		    (setf (gethash (first line) hash-lines) line)	;start point
		    (setf (gethash (second line) hash-lines) line)) ;end point
		  )))
	 (when (get-debug-mode)	 (format t "hash points: ~a~%" hash-points)))
    hash-lines))

;write better
(defun slope-match? (line1 line2)
  "Returns T if screw of two lines match and nil otherwise."
  (let ((angle1 (get-tilt-angle line1))
	(angle2 (get-tilt-angle line2)))
    (cond
      ((>= +max-slope-angle+ (abs (- angle1 angle2))) t)
      (t nil))))

(defun merge-near-lines (line-hash &key (radius 3))
  "Find near lines and merge them. Returns new hash with lines."
  (flet ((get-points (point radius)
	   (let ((x (first point))
		 (y (second point))
		 points-list)
	     (loop for i from (- x radius) to (+ x radius) do
		  (loop for j from (- y radius) to (+ y radius) do
		       (let ((dx (- i x))
			     (dy (- j y)))
			 (when (and 
				(not (equal (cons i j) (cons x y))) 
				(>= radius (sqrt (+ (* dx dx) (* dy dy))))
				(push (list i j) points-list))))))
	     points-list)))
    (let ((new-lines-hash (make-hash-table :test 'equal ))
	  (total-merged 0)
	  points-list 
	  cur-line
	  key-line)

      (loop for point being the hash-key of line-hash do
	   (setf points-list (get-points point radius))
	   (setf key-line (gethash point line-hash))
	   (dolist (pt points-list) 
	     (setf cur-line (gethash pt line-hash nil))
	     (when (and 
		    (not (eq nil cur-line))
		    (not (equal key-line cur-line))
		    (slope-match? key-line cur-line))
	       ;; (format t "merge lines: ~%Line1 = ~a~%Line2 = ~a~%~%" 
	       ;; 	       (get-line-string key-line)
	       ;; 	       (get-line-string cur-line))

	       (remhash point line-hash)
	       (remhash (second key-line) line-hash)
	       (remhash pt line-hash)
	       (remhash (second cur-line) line-hash)
	       (setf key-line (merge-two-lines key-line cur-line))
	       (incf total-merged)
	       (return)))		; get next point from hash-key
	   (setf (gethash (first key-line) new-lines-hash) key-line)
	   (setf (gethash (second key-line) new-lines-hash) key-line))

      (when (get-debug-mode) (format t "Total ~a lines merged~%" (* 2 total-merged)))
      new-lines-hash)))



