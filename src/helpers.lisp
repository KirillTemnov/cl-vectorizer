
(in-package #:cl-vectorizer)

(defun print-hash (hash)
  "print hash table keys and values"
  (maphash #'(lambda (key value)  (format t "The value associated with the key ~S is ~S~%" key value))
 hash))


(defun tokenize (string)
  "Split string by space to list of srings"
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
  "Write content to file (filename). Overwrite existing file"
  (with-open-file (stream  filename 
			   :direction :output
			   :if-exists :supersede
			   :if-does-not-exist :create)
    (format stream content))
  filename)

(defun mapconcat (func elems)
  "Execute function on each of elems and concatenate all results in a string."
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
  "Concatenate path to IN working dir and filename."
  (merge-pathnames (getf *settings* :working-dir-in) filename))

(defun get-out-path (filename)
  "Concatenate path to OUT working dir and filename."
  (merge-pathnames (getf *settings* :working-dir-out) filename))

(defun get-out-path-make-dir (filename)
  "Concatenate path to OUT working dir and filename.
Creates OUT working directory, if it doen't exist."
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
  "Add `add-symbols` value to filename

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

(defun hashtable-to-image (hashtable width height &key (color +black+) (bg-color +white+))
  "Create image from hash table"
  (let ((image (png:make-image height width 1))
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
  "Create list from hash table keys."
  (let (lst)
    (loop for key being the hash-key of hash do
	 (Push key lst))
    lst))

(defun remove-list-element-from-hash (list hash)
  (dolist (i list)
    (remhash i hash)))

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

(defun push-to-list (list &rest elems )
  "Push all elements to list."
  (dolist (i elems)
    (push i list))
  list)

(defun push-to-list-if-not-present (list &rest elems)
  "Push elements to list if they don't already in `list`."
  (dolist (i elems)
    (when (not (member i list :test #'equal))
      (push i list)))
  list)

