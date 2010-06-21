
(in-package #:cl-vectorizer)

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
;;    ((> 2 (get-points-distance p1 p2)) nil)
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
    ((or (not (listp x)) (not (listp y))) nil)
    ((eq (first x) (first y))
     (> (second x) (second y)))
    (t
     (< (first x) (first y)))))


(defun get-tilt-angle (line)
  "Returns tilt angle between line and horizont. Result return in degrees."
  (let* ((p1 (first line))
	 (p2 (second line))
	 (dx (- (first p1) (first p2)))
	 (dy (- (second p1) (second p2))))
    (cond
      ((= 0 dx) 90)
      (t (rad-to-degree (atan (/ dy dx)))))))


(defun merge-two-lines (line1 line2)
  "Merge two lines in one"
  (let*
      ((points (sort (list (first line1) (second line1) (first line2) (second line2)) #'compare-points))
       (start (first points))
       (end (fourth points))
      (result (make-line start end)))
    (when (get-debug-mode)
      (when  (< (third result) (+ (third line1) (third line2)))
	(format t "Merge lines ~a and ~a~%" (get-line-string line1) (get-line-string line2))
	(format t "Result: ~a~%" result)))
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
    (> (get-max-angle-on-line) (substitute-angles tilt-angle new-tilt)))) ; 15 degrees 

;; Что нужно сделать?
;; 1) Получить первую точку из массива точек у которой будет 1 сосед.
;;    Это будет начало новой линии. Удалить точку из хеша.
;; 2) Найти соседнюю точку и получить направление.
;;    Направление не может меняться более чем на 90 грудусов.
;;    Удалить точку из хеша.
;; 3) Если у соседней точки нет соседей или направление сильно меняется - добавить новую линию,
;;    иначе перейти к п 2)
;; Закончить когда больше не останется точек

;;tip: (member elem list :test #'equal)

(defun find-end-of-line (point hash-points &key (line nil) (tilt-angle nil))
  "Search and of line. Store current line in `line`."
  (let* ((active-points (get-active-points point line hash-points))
	 (start-point (first (last line))))
    
    (cond

      ((eq nil line)			;create new line
       (cond ((= 0 (length active-points))
	      (progn
		(remhash point hash-points)
		nil))
	     (t
	      (progn
		(push point line)
		(find-end-of-line (first active-points) hash-points :line line :tilt-angle tilt-angle)))))

      ((=  0 (length active-points))		; end of line
       (progn
	 (push point line)
	 (remove-list-element-from-hash line hash-points)
	 (make-line start-point (first line))))

      ((>= 3 (length active-points)) ; too match neibhours, remove point finish line
       (progn 
	 (push point line)
	 (remove-list-element-from-hash line hash-points)
	 (make-line start-point (first line))))

      ((>= 3 (length line))	; first 3 points
       (progn
	 (push point line)
	 (when (= 3 (length line))
	   (setf tilt-angle (get-tilt-angle (list start-point point))))
	 (find-end-of-line (first active-points) hash-points :line line :tilt-angle tilt-angle)))
      
      (t				; line length > 3 point
	 (if (point-belong-to-line? point start-point (first line) tilt-angle)
	     (progn
	       (push point line)
	       (find-end-of-line (first active-points) hash-points :line line :tilt-angle tilt-angle))

	     (progn
	       (remove-list-element-from-hash line hash-points)
	       (make-line start-point (first line))))))))

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
	      ;; (when (point-have-one-neibhour? point hash-points)
	      ;; 	(progn
	      (setf line (find-end-of-line point hash-points))
	      (when (and (not (line? line)) (get-debug-mode))
		(format t "point = ~a    line = ~a~%" point line))
	      (when (line? line) 
		(when (get-debug-mode) (format t "add line ") (print-line line))
		;; (progn
		;;(print-line line)
		(setf (gethash (first line) hash-lines) line)	;start point
		(setf (gethash (second line) hash-lines) line)) ;end point
	      )
	 (when (get-debug-mode)	 (format t "hash points: ~a~%" hash-points)))
    hash-lines))

;;TODO write it better
(defun slope-match? (line1 line2)
  "Returns T if screw of two lines match and nil otherwise."
  (let ((angle1 (get-tilt-angle line1))
	(angle2 (get-tilt-angle line2)))
    (cond
      ((>= (get-max-slope-angle) (abs (- angle1 angle2))) t)
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
      (when (get-debug-mode)
	(format t "Merge near lines ...~%~%")) 

      (loop for point being the hash-key of line-hash do
	   (setf points-list (get-points point radius))
	   (setf key-line (gethash point line-hash))
	   (dolist (pt points-list) 
	     (setf cur-line (gethash pt line-hash nil))
	     (when (and 
		    (not (eq nil cur-line))
		    (not (equal key-line cur-line))
		    (slope-match? key-line cur-line))
	       (when (get-debug-mode)
		 (format t "merge lines: ~%Line1 = ~a~%Line2 = ~a~%~%" 
			 (get-line-string key-line)
			 (get-line-string cur-line)))

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



