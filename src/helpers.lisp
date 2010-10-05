
(in-package #:cl-vectorizer)

(defun print-hash (hash)
  "Print HASH keys and values."
  (maphash #'(lambda (key value)  (format t "The value associated with the key ~S is ~S~%" key value))
 hash))

(defun get-max-coordinates (hash-points)
  "Get maximum X and Y coordinates.
 Returned values may not belong to a single point."
  (let ((max-x 0) (max-y 0))
    (loop for point being the hash-key of hash-points
       do
         (unless (null point)
           (when (< max-x (first point))
             (setf max-x (first point)))
           (when (< max-y (second point))
             (setf max-y (second point)))))
    (list max-x max-y)))


(defun tokenize (string)
  "Split STRING by space to list of srings."
  (let ((pos (min (or (position #\Space string) (1- (length string)))
		  (or (position #\Tab string) (1- (length string))))))
    (cond
      ((zerop (length string)) nil)
      ((or (char= #\Space (aref string 0)) (char= #\Tab (aref string 0)))
       (tokenize (subseq string 1)))
      ((and (numberp pos) (plusp pos))
       (cons (string-trim '(#\Space #\Tab) (subseq string 0 (1+ pos)))
	     (tokenize (subseq string (1+ pos)))))
      (t (list (string-trim '(#\Space #\Tab) string))))))

(defun write-to-file (content filename)
  "Write CONTENT to file FILENAME. Overwrites existing file."
  (with-open-file (stream  filename
			   :direction :output
			   :if-exists :supersede
			   :if-does-not-exist :create)
    (format stream content))
  filename)

(defun mapconcat (func elems)
  "Execute FUNC on each of ELEMS and concatenate all results in a string."
  (cond
    ((eq nil elems) "")
    (t
     (concatenate 'string
		  (funcall func (first elems))
		  (mapconcat func (cdr elems))))))

(defun in-hash(key hash)
  "Check if key in hash."
  (not (eq nil (gethash key hash nil))))

(defun get-hash-point-value (point hash)
  "Return value of point in hash."
  (cond
   ((eq (in-hash point hash) nil) 0)
   ('t 1)))


(defun str-list-to-int-list (list-string)
  "converst string with decimal digits to list of decimal digits
sample (\"1\" \"2\" \"3\")  -> (1 2 3)"
  (map 'list #'(lambda (elem)  (parse-integer elem :junk-allowed t)) (tokenize list-string)))

(defun get-in-path (filename)
  "Concatenate path to *WORKING-DIR-IN* working dir and filename."
  (merge-pathnames (getf *settings* :working-dir-in) filename))

(defun get-out-path (filename)
  "Concatenate path to *WORKING-DIR-OUT* working dir and filename."
  (merge-pathnames (getf *settings* :working-dir-out) filename))

(defun get-out-path-make-dir (filename)
  "Concatenate path to *WORKING-DIR-OUT* working dir and FILENAME.
Creates *WORKING-DIR-OUT* working directory, if it doen't exist."
  (let ((file-path (merge-pathnames (getf *settings* :working-dir-out) filename)))
    (with-open-file (temp-file
		     (ensure-directories-exist file-path) :direction :output  :if-exists :supersede))
    (delete-file file-path)
    file-path))

(defun change-extension (filename new-extension)
  "Change extension of filename.
Example:
  (change-extension #p\"test-file.name\" \"ext\")
  -> #p\"test-file.ext\"
"
  (make-pathname :name (pathname-name filename)
		 :type new-extension
		 :directory (pathname-directory filename)))

(defun change-filename (filename new-filename)
  "Change name of file and keep extension"
  (make-pathname :name new-filename
		 :type (pathname-type filename)
		 :directory (pathname-directory filename)))

(defun add-to-filename (filename add-symbols)
  "Add ADD-SYMBOLS value to FILENAME

Example:
> (add-to-filename #p\"/tmp/test.txt\" 5)

  #P/tmp/test5.txt
"
  (make-pathname :name (format nil "~a~a" (pathname-name filename) add-symbols)
		 :type (pathname-type filename)
		 :directory (pathname-directory filename)))

(defun load-image (path)
  "Load image from file"
  (png:decode (open path :element-type '(unsigned-byte 8))))

(defun save-image (image path)
  "Save image to file"
  (with-open-file (output path :element-type '(unsigned-byte 8)
			  :direction :output :if-exists :supersede)
        (png:encode image output)))


(defun image-to-hashtable (image)
  "Convert image to hash table."
  (let* ((w (png:image-width image))
	 (h (png:image-height image))
	 (threshold (get-threshold))
	 (hash-key nil)
	 (ht (make-hash-table :test 'equal :size (floor (* 0.1 w h)))))
    (loop for i from 0 to (1- w) do
	   (loop for j from 0 to (1- h) do
		(when (> threshold (aref image j i 0))
		  (setf hash-key (list i j))
		  (setf (gethash hash-key ht) +black+))))
    ht))

(defun hashtable-to-image (hashtable  &key (color +black+) (bg-color +white+))
  "Create image from hash table"
  (let* ((max-coordinates (get-max-coordinates hashtable))
         (width (first max-coordinates))
         (height (second max-coordinates))
         (image (png:make-image height width 1))
	(hash-key nil))
    (loop for i from 0 to (1- width) do
	 (loop for j from 0 to (1- height) do
	      (setf hash-key (list i j))
	      (setf (aref image j i 0)
		    (cond
		      ((in-hash hash-key hashtable) color)
		      (t bg-color)))))
    image))

(defun get-neibhour-points (point hash-points)
  "Get neibhours points near specified point.
   p9  p2  p3
   p8  p1  p4
   p7  p6  p5

   p1 is a specified point. Point returns in list '(p2 p3 p4 p5 p6 p7 p8 p9)
"
  (let 	((p2 (get-hash-point-value (list (first point)  (1- (second point))) hash-points))
	 (p3 (get-hash-point-value (list (1+ (first point))  (1- (second point))) hash-points))
	 (p4 (get-hash-point-value (list (1+ (first point)) (second point)) hash-points))
	 (p5 (get-hash-point-value (list (1+ (first point)) (1+ (second point))) hash-points))
	 (p6 (get-hash-point-value (list (first point) (1+ (second point))) hash-points))
	 (p7 (get-hash-point-value (list (1- (first point)) (1+ (second point))) hash-points))
	 (p8 (get-hash-point-value (list (1- (first point)) (second point)) hash-points))
	 (p9 (get-hash-point-value (list (1- (first point)) (1- (second point))) hash-points)))
    (list p2 p3 p4 p5 p6 p7 p8 p9)))

(defun get-list-neibhours (point)
  "Returns list of all neibhour point coordinates in order p2... p9 (see get-neibhour-points).
   Example:
   point = '(3 7)
   (2 6)  (3 6)  (4 6)
   (2 7)  (3 7)  (4 7)
   (2 8)  (3 8)  (4 8)
   will return '((3 6) (4 6) (4 7) (4 8) (3 8) (2 8) (2 7) (2 6))
"
  (list (list (first point)  (1- (second point)))
	(list (1+ (first point))  (1- (second point)))
	(list (1+ (first point)) (second point))
	(list (1+ (first point)) (1+ (second point)))
	(list (first point) (1+ (second point)))
	(list (1- (first point)) (1+ (second point)))
	(list (1- (first point)) (second point))
	(list (1- (first point)) (1- (second point)))))

(defun get-neibhour-active-points (point hash-points)
  "Return list with neibhours coordinates.
   Example:
   p1 = '(3 3)

   0  0  0
   0  1  1
   0  1  0
   will return '((4 3) (3 4))
"
  (let ((neibhours nil))
    (dolist (p (get-list-neibhours point))
      (when (in-hash p hash-points)
	(push p neibhours)))
    neibhours))

(defun get-active-points (point line hash-points)
  "Get list of neibhour points, exclude point that belongs to `line`."
  (let ((ap (get-neibhour-active-points point hash-points)) ap-list)
    (dolist (p ap)
      (when (not (member p line :test #'equal))
	(push p ap-list)))
    ap-list))

(defun should-delete-point? (point hash-points)
  "Check if point should be deleted.
   This check consists of three parts:
   1) Point have 0 neibhours.
   2) Point have 2 neibhours and this neibhours defined by one of four masks:
       Legend:
       p9  p2  p3
       p8  p1  p4
       p7  p6  p5

       Masks (x2):
       0  1  0    0  1  0    0  0  0    0  0  0
       1  1  0    0  1  1    0  1  1    1  1  0
       0  0  0    0  0  0    0  1  0    0  1  0

       Masks (x3):
       1  1  0    0  1  0    0  1  0    0  1  1
       0  1  1    1  1  0    0  1  1    1  1  0
       0  0  0    1  0  0    0  0  1    0  0  0

       0  0  0    0  0  1    1  0  0    0  0  0
       1  1  0    0  1  1    1  1  0    0  1  1
       0  1  1    0  1  0    0  1  0    1  1  0

   Point should be deleted of one of conditions performed.
"
  (let* ((points (get-neibhour-points point hash-points))
	 (sum (apply #'+ points)))
    (or
     (= 0 sum)
     (and (= 2 sum)
	  (or
	   (equal points '(1 0 0 0 0 0 1 0))
	   (equal points '(1 0 1 0 0 0 0 0))
	   (equal points '(0 0 1 0 1 0 0 0))
	   (equal points '(0 0 0 0 1 0 1 0))))
     (and (= sum 3)
	  (or
	   (equal points '(1 0 1 0 0 0 0 1))
	   (equal points '(1 0 0 0 0 1 1 0))
	   (equal points '(1 0 1 1 0 0 0 0))
	   (equal points '(1 1 0 0 0 0 1 0))
	   (equal points '(0 0 0 1 1 0 1 0))
	   (equal points '(0 1 1 0 1 0 0 0))
	   (equal points '(0 0 0 0 1 0 1 1))
	   (equal points '(0 0 1 0 1 1 0 0)))))))

(defun rad-to-degree (value)
  "Convert radians to degrees."
  (/ (* 180 value) pi))

(defun degree-to-rad (value)
  "Convert degrees to radians."
  (/ (* pi value) 180))

(defun get-points-distance (p1 p2)
  "Returns eucleadian distance between 2D points p1 and p2"
  (let* ((x1 (first p1))
	 (y1 (second p1))
	 (x2 (first p2))
	 (y2 (second p2))
	 (dx (- x2 x1))
	 (dy (- y2 y1)))
    (sqrt (+ (* dx dx)  (* dy dy)))))

(defun hashtable-keys-to-list (hash)
  "Create list from HASH keys."
  (let (lst)
    (maphash #'(lambda (key value)
                 (declare (ignore value))
                 (push key lst))
             hash)
    lst))

(defun hashtable-values-to-list (hash)
  "Create list from HASH values."
  (let (lst)
    (maphash #'(lambda (key value)
                 (declare (ignore key))
                 (push value lst))
             hash)
    lst))


(defun remove-list-element-from-hash (list hash)
  (dolist (i list)
    (remhash i hash)))

(defun get-line-length (line)
  "Returns length of 2D line.
   Example:
   line = '((3 3) (11 4) length)
   will return 8.062258"
  (get-points-distance (first line) (second line)))

(defun center-point (line)
  "Calculates center point of line (section).
Example:
 (center-point '((10 10) (20 20)))
=> '(15 15)"
  (let ((p1 (first line))
	(p2 (second line)))
    (list (round (/ (+ (first p1) (first p2)) 2))
	  (round (/ (+ (second p1) (second p2)) 2)))))

(defun min-distance (line1 line2)
  "Get minimum distance from one of end points of LINE1 to
 end point of line LINE2."
  (let ((p1 (first line1))
        (p2 (second line1))
        (p3 (first line2))
        (p4 (second line2)))
    (min (get-points-distance p1 p3) (get-points-distance p1 p4)
         (get-points-distance p2 p3) (get-points-distance p2 p4))))

(defun hashlines-to-hashpoints (hash-lines)
  "Extract points from `hash-lines` and return its as hashtable with points as keys."
  (let ((hash-points (make-hash-table :test #'equal)))
    (maphash #'(lambda (key line)
		 (when (line? line)
		   (setf (gethash (first line) hash-points) 1)
		   (setf (gethash (second line) hash-points) 1)
		   (setf (gethash (center-point line) hash-points) 1)))
	     hash-lines)
    hash-points))



;; (defun get-other-line-point (line point)
;;   "Return another line point."
;;   (cond
;;     ((equal point (first line)) (second line))
;;     ((equal point (second line)) (first line))
;;     (t
;;      (error "Point not belong to line"))))

(defun remove-hash-lines-duplicates (hash-lines)
  "Remove duplicate key entries, points to one line."
  (let (line)
    (loop for point being the hash-key of hash-lines do
	 (setf line (gethash point hash-lines nil))
	 (when (line? line)
	   ;; remove lines, shorter than (get-max-noise-line-length)
	   (when (>= (get-max-noise-line-length) (third line))
	     (remhash (first line) hash-lines))
	   (remhash (second line) hash-lines)))
    hash-lines))

(defun push-to-list (list &rest elements )
  "Push all ELEMENTS to LIST."
  (dolist (i elements)
    (push i list))
  list)

(defun push-to-list-if-not-present (list &rest elements)
  "Push ELEMENTS to list if they don't already in LIST."
  (dolist (i elements)
    (when (not (member i list :test #'equal))
      (push i list)))
  list)

(defun avg (&rest arguments)
  "Average value of list ARGUMENTS."
  (/ (apply #'+ arguments) (length arguments)))


(defun merge-lists-remove-duplicates (list1 &rest lists)
  "Merge two lists and remove duplicates from resulting list.
Example:

 (let ((l1 '(1 2 3))
      (l2 '(a b c))
      (l3 '(q1 w2 r4)))
  (merge-lists-remove-duplicates l1 l2 l3 '(ZZZ zz z)))

=> (R4 W2 Q1 C B A 1 2 3)"
  (dolist (list2 lists)
    (dolist (i list2)
      (when (not (member i list1 :test #'equal))
	(push i list1))))
  list1)

(defun vector-min (vector)
  "Get minimum value from VECTOR. If vector is empty, return nil."
  (if (< 0 (length vector))
    (let ((min-value (aref vector 0)))
      (loop for i from 0 to (1- (length vector)) do
           (when (< (aref vector i) min-value)
             (setf min-value (aref vector i))))
      min-value)
    nil))

(defun vector-max (vector)
  "Get maximum value from VECTOR. If vector is empty, return nil."
  (if (< 0 (length vector))
      (let ((max-value 0))
        (loop for i from 0 to (1- (length vector)) do
             (when (< max-value (aref vector i))
               (setf max-value (aref vector i))))
        max-value)
      nil))

(defun generate-near-points (point distance)
  "Generate near points for POINT. Near points are in square form -DISTANCE  to +DISTANCE
from the POINT."
  (let ((x (first point))
        (y (second point))
        points)
    (loop for x from (- x distance) to (+ x distance) do
         (loop for y from (- y distance) to (+ y distance) do
              (push (list x y) points)))
    points))


(defun filter-hash (hash predicate)
  "Filter HASH values and return new hash, with values, satisfied to PREDICATE."
  (let ((ht (make-hash-table :test #'equal)))
    (maphash #'(lambda (key value)
                 (when (funcall predicate value)
                   (setf (gethash key ht) value)))
             hash)
    ht))

(defun inverse-y (point)
  "Inverse y value of a POINT."
  (list (first point) (- (second point))))

;; todo merge dxf and svg managers
(defun hashtable-lines-to-dxf-manager (ht manager)
  "Save lines from hash keys to dxf manager"
  (loop for point being the hash-key of ht
       using (hash-value line)  do
       (when (line? line)
         (sb-dxf:add-object manager (make-instance 'sb-dxf:dxf-line
                                            :start-point (inverse-y (first line))
                                            :end-point (inverse-y (second line))))))
  manager)


(defun hashtable-lines-to-svg-manager (ht manager &key
                                       (short-lines-color "blue")
                                       (long-lines-color "green"))
  "Save lines from hash keys to svg manager."
  (let (line color (lines 0) (short-lines 0))
    (loop for point being the hash-key of ht
       using (hash-value line) do
	 (when (line? line)
	   (incf lines)
           (if (< (get-line-length line) (get-max-small-line-length))
               (progn
                 (setf color short-lines-color)
                 (incf short-lines))
               (setf color long-lines-color))
	   (add-entity manager (make-svg-line line :color color))))

;;    (when (get-debug-mode)
    (format t "Total ~A lines pushed to svg manager (~A short lines).~%" lines short-lines)
    manager))

(defun hashtable-circles-to-svg-manager (circles manager)
  "Save circles from hash keys to svg manager."
  (loop for circle being the hash-key of circles do
       (add-entity manager (make-svg-circle circle :color "blue" :width 2)))
  (when (get-debug-mode)
    (format t "Total ~a circles pushed to svg manager.~%" (hash-table-count circles)))
  manager)

(defun list-points-to-svg-manager (points-list manager)
  "Save points from list to svg manager."
  (dolist (pt points-list)
    (when (and  (listp pt) (= 2 (length pt)))
      (add-entity manager (make-svg-point pt :color "magenta"))))
  (when (get-debug-mode)
    (format nil "Total ~a points pushed to svg manager.~%" (length points-list)))
  manager)


(defun draw-grid (manager grid-size width height &key (color "black") (offset-x 1) (offset-y 1))
  "Create grid on managed canvas with GRID-SIZE on square from '(0 0) to (WIDTH HEIGHT)."
  (let ((x offset-x) (y offset-y) line)
    (loop while (< x width) do
         (setf line (make-instance 'svg-line :point1 (list x 0) :point2 (list x height) :stroke color))
         (add-entity manager line)
         (incf x grid-size))

    (loop while (< y height) do
         (setf line (make-instance 'svg-line :point1 (list 0 y) :point2 (list width y) :stroke color))
         (add-entity manager line)
         (incf y grid-size))))



  ;; (loop repeat (1- (floor (/ width grid-size)))
  ;;    for x = 0 then (+ x grid-size) do
  ;;      (progn
  ;;      (loop repeat (1- (floor (/ height grid-size)))
  ;;         for y = 0 then (+ y grid-size)) do
  ;;         (let ((line (make-instance 'svg-line :point1 (list x y) :point2 (list (+ x grid-size) (+ y grid-size)))))
  ;;           (add-entity manager line)))))


#|
 (defun duplicate (item times)
  "Duplicate ITEM TIMES times and return RESULT as a list"
  (let ((result))
    (format t "times = ~A~%" times)
    (loop while (< 0 times) do
         (decf times)
         (push item result))
    result))
|#
