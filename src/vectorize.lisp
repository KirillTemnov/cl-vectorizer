
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
  (format nil "#Line {~a} ~%" line))

(defun <point (p1 p2)
  "Points comparsing function. Returns T if x of P1 is less than x of P2,
or if x'es of P1 and P2 are equal, but y of P1 is less than y of P2."
  (or
   (< (first p1) (first p2))
   (and
    (= (first p1) (first p2))
    (< (second p1) (second p2)))))

(defun <line (line1 line2)
  "Lines comparsing function. Returns T if first point of LINE1 is
less, than first point of LINE2. If first points are equal, function compare second points."
  (or
   (<point (first line1) (first line2))
   (and
    (equal (first line1) (first line2))
    (<point (second line1) (second line2)))))

(defun make-line (p1 p2)
  "Create line which starts from left (0) to right (infinity)"
  (cond
    ;;    ((> 2 (get-points-distance p1 p2)) nil)
    ((<point p1 p2) (list p1 p2 (get-points-distance p1 p2)))
    (t (list p2 p1 (get-points-distance p1 p2)))))

(defun get-lines-min-distance (line1 line2)
  "Get minimum distance between LINE1 and LINE2 end points."
  (min (get-points-distance (first line1) (first line2))
       (get-points-distance (second line1) (first line2))
       (get-points-distance (first line1) (second line2))
       (get-points-distance (second line1) (second line2))))

(defun get-tilt-angle (line)
  "Returns tilt angle between line and horizont. Result return in degrees."
  (let* ((p1 (first line))
         (p2 (second line))
         (dx (- (first p1) (first p2)))
         (dy (- (second p1) (second p2)))
         (dg (if (= 0 dx)
                 90
                 (rad-to-degree (atan (/ dy dx))))))
    ;; correct value, beacause atan range is [-pi/2; pi/2]
    (cond
      ((and (< 0 dx))
       (+ 180 dg))
      ((and (> 0 dx) (> 0  dg))
       (+ 360 dg))
      (t
       dg))))

(defun compare-points (x y)
  "Compare two points, point less if it plased from left and(or) above another point."
  (cond
    ((or (not (listp x)) (not (listp y))) nil)
    ((eq (first x) (first y))
     (> (second x) (second y)))
    (t
     (< (first x) (first y)))))


(defun one-line-lies-on-the-other (line1 line2 &key (permissible-error 1))
  "Compares two lines, and if one of them lay on the other, return line,
that bigger, otherwise return nil."
  (labels ((line2-lie-on-line1 (line1 line2 p-err)
             (let* ((p1 (first line1))
                    (p2 (second line1))
                    (p3 (first line2))
                    (p4 (second line2)))
               (and
                (<= (- (first p1) p-err) (first p3) (first p4) (+ (first p2) p-err))
                (<= (- (min (second p1) (second p2)) p-err)
                    (min (second p3) (second p4))
                    (max (second p3) (second p4))
                    (+ (max (second p1) (second p2)) p-err))))))
    (cond
      ((line2-lie-on-line1 line1 line2 permissible-error) line1)
      ((line2-lie-on-line1 line2 line1 permissible-error) line2)
      (t nil))))


(defun merge-two-lines (line1 line2)
  "Merge 2 lines. Compare 4 distances between points."
  (let ((source-line (one-line-lies-on-the-other line1 line2)))
    (if source-line
        source-line
        (let ((angle1 (get-tilt-angle line1))
              (angle2 (get-tilt-angle line2))
              (p1 (first line1))
              (p2 (second line1))
              (p3 (first line2))
              (p4 (second line2)))
          (cond
            ((< (abs (- angle1 angle2)) 2) ; angle between lines < 2 degrees, just merge them
             ;; (format t "ML: almost parallel~%"  )
             (let* ((d1 (get-points-distance p1 p3))
                    (d2 (get-points-distance p1 p4))
                    (d3 (get-points-distance p2 p3))
                    (d4 (get-points-distance p2 p4))
                    (sorted-distances
                     (sort (list (list p1 p3 d1) (list p1 p4 d2)
                                 (list p2 p3 d3) (list p2 p4 d4))
                           #'(lambda (elem1 elem2) (> (third elem1) (third elem2))))))
               (make-line (first (first sorted-distances)) (second (first sorted-distances)))))
            ;; else, make one of lines longer
            ;; todo work on this code
            ((<line line1 line2)
             ;; (format t "ML: ~A < ~A~%" line1 line2)
             (make-line-longer p1 p2
                               (+ (get-lines-min-distance line1 line2) (third line2))))
            (t
             ;; (format t "ML: ~A <= ~A~%" line2 line1)
             (make-line-longer p2 p1
                               (+ (get-lines-min-distance line1 line2) (third line2)))))))))

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
    (cond
      ((< +max-line-len+ distance)
       (> 0.5 (substitute-angles tilt-angle new-tilt)))
      (t
       (> (get-max-angle-on-line) (substitute-angles tilt-angle new-tilt)))))) ; 15 degrees

(defun find-end-of-line (point hash-points &key (line nil) (tilt-angle nil))
  "Search and of line. Store current line in `line`."
  (let* ((active-points (get-active-points point line hash-points))
         (start-point (first (last line))))

    (cond

      ((eq nil line)   ;create new line
       (cond ((= 0 (length active-points))
              (progn
                (remhash point hash-points)
                nil))
             (t
              (progn
                (push point line)
                (find-end-of-line (first active-points) hash-points :line line :tilt-angle tilt-angle)))))

      ((=  0 (length active-points))  ; end of line
       (progn
         (push point line)
         (remove-list-element-from-hash line hash-points)
         (make-line start-point (first line))))

      ((<= 3 (length active-points)) ; too match neibhours, remove point finish line
       (progn
         (push point line)
         (remove-list-element-from-hash line hash-points)
         (make-line start-point (first line))))

      ((>= +min-line-len+ (length line)) ; first 3 points
       (progn
         (push point line)
         (when (= 3 (length line))
           (setf tilt-angle (get-tilt-angle (list start-point point))))
         (find-end-of-line (first active-points) hash-points :line line :tilt-angle tilt-angle)))

      (t    ; line length > 3 point
       (if (point-belong-to-line? point start-point (first line) tilt-angle)
           (progn
             (push point line)
             (find-end-of-line (first active-points) hash-points :line line :tilt-angle tilt-angle))

           (progn
             (remove-list-element-from-hash line hash-points)
             (make-line start-point (first line))))))))

(defun point-have-one-neibhour? (point hash-points)
  "Returns T if POINT have only one neibhour."
  (= 1 (apply #'+ (get-neibhour-points point hash-points))))

(defun point-have-two-or-more-neibhours?  (point hash-points)
  "Returns T in POINT have >= 2 neibhours."
  (<= 2 (apply #'+ (get-neibhour-points point hash-points))))

(defun vectorize-hash (hash-points)
  "Vectorize hash with points and return hash, consists of lines (as keys)."
  (let ((hash-lines (make-hash-table :test 'equal ))
        (line nil) (hash-len (1+ (hash-table-count hash-points))))
    (loop while
         (and
          (> hash-len (hash-table-count hash-points))
          (< 0 (hash-table-count hash-points)))
       do
         (setf hash-len (hash-table-count hash-points))

         (loop for point being the hash-key of hash-points do
            ;; (when (point-have-one-neibhour? point hash-points)
            ;;  (progn
              (setf line (find-end-of-line point hash-points))
              (when (and (not (line? line)) (get-debug-mode))
                (format t "point = ~a    line = ~a~%" point line))
              (when (line? line)
                (when (get-debug-mode) (format t "add line ") (print-line line))
                (setf (gethash (first line) hash-lines) line) ;start point
                (setf (gethash (second line) hash-lines) line)) ;end point
              )
         (when (get-debug-mode)  (format t "hash points: ~a~%" hash-points)))
    hash-lines))

(defun vectorize-hash->points (hash-points)
  "Vectorize HASH-POINTS to lines and add middle points of lines in resulting hash."
  ;;  (let ((hash-lines (vectorize-hash hash-points)))
  ;; (loop for point being the hash-key of hash-points
  ;; using (hash-value line) do
  ;;   (when (line? line)
  ;;     (let ((center (list
  ;;                    (round (/ (+ (first (first line)) (first (second line))) 2))
  ;;                    (round (/ (+ (second (first line)) (second (second line))) 2)))))
  ;;       (setf (gethash center hash-lines) t))))
  (loop for point being the hash-key of hash-points do
       (when (point-have-two-or-more-neibhours? point hash-points)
         (remhash point hash-points)))
  hash-points)

;;TODO write it better
(defun can-merge? (line1 line2)
  "Returns T if lines can be merged and nil otherwise."
  (let ((angle1 (get-tilt-angle line1))
        (angle2 (get-tilt-angle line2)))
    (if
      (and
        (< (abs (- angle1 angle2)) (get-max-slope-angle))
        (or
         (< (get-max-length-to-restore) (get-points-distance (first line1) (first line2)))
         (< (get-max-length-to-restore) (get-points-distance (second line1) (first line2)))))
        t
        nil)))


(defun merge-near-lines (line-hash)
  "Find near lines and merge them. Returns new hash with lines."
  (labels ((get-points (line)
           (let ((x1 (first (first line)))
                 (y1 (second (first line)))
                 (x2 (first (second line)))
                 (y2 (second (second line)))
                 (radius (get-line-search-radius))
                 points-list)
             (loop for i from (- x1 radius) to (+ x1 radius) do
                  (loop for j from (- y1 radius) to (+ y1 radius) do
                       (let ((dx (- i x1))
                             (dy (- j y1)))
                         (when (and
                                (not (equal (cons i j) (cons x1 y1)))
                                (not (equal (cons i j) (cons x2 y2)))
                                (>= radius (sqrt (+ (* dx dx) (* dy dy))))
                                (push (list i j) points-list))))))
             points-list)))
    (let ((new-lines-hash (make-hash-table :test 'equal))
          (total-merged 10)
          points-list
          cur-line)

      (loop for point being the hash-key
         using (hash-value key-line) of line-hash do
           (when (<= 3 (third key-line)) ; not regard small lines
             (setf points-list (get-points key-line))
             (dolist (pt points-list)
               (setf cur-line (gethash pt line-hash nil))
               (when (and
                      (not (eq nil cur-line))
                      (not (equal key-line cur-line))
                      (can-merge? key-line cur-line))
                 ;; (when (get-debug-mode)
                 ;;   (format t "merge lines: ~%Line1 = ~a~%Line2 = ~a~%~%"
                 ;;    (get-line-string key-line)
                 ;;    (get-line-string cur-line)))

                 (remhash point line-hash)
                 (remhash (second key-line) line-hash)
                 (remhash pt line-hash)
                 (remhash (second cur-line) line-hash)
                 (setf key-line (merge-two-lines key-line cur-line))
                 (incf total-merged)
                 (return)))  ; get next point from hash-key

             (setf (gethash (first key-line) new-lines-hash) key-line)
             (setf (gethash (second key-line) new-lines-hash) key-line)))

      (loop while (< 0 (merge-lines new-lines-hash)) do
           (format t "up.~%"  ))
      new-lines-hash)))
;;      line-hash)))



(defun hough-lines-table (points &key (angle-delta 5))
  "Calculate hough lines table for POINTS from 0 to 180 degrees with step ANGLE-DELTA.
Return table as a list of lists. First element of nested list is angle, other elements are
distances between '(0 0) line to corresponding point with angle specified as a first
element."
  (let (distance-table)
    (loop for angle = 0 then (+ angle  (degree-to-rad angle-delta))
       while (< angle pi) do
         (push
          (list (rad-to-degree angle)
                (mapcar #'(lambda (point)
                            (+ (* (first point) (cos angle)) (* (second point) (sin angle))))
                        points))
          distance-table))
    distance-table))

(defun merge-lines (lines-hash)
  "Merge near lines, that belongs to same straight line and not far from each other."
  (let ((total-lines-merged 0)
        (max-distance 10)
        lines-for-merging)
    (loop for point being the hash-key of lines-hash
       using (hash-value line) do
         (when (and
                (< (get-max-small-line-length) (third line))
                (not (member line lines-for-merging :test #'equal)))
           (let ((bbox (line-bounding-box line :margin (min max-distance (third line))))
                 (lines-list (list line)))
             ;; take all lines placed in bbox and put them to lines-list
             (loop for anything being the hash-key
                using (hash-value some-line) of lines-hash do
                  (when (and
                         (inside-box? some-line bbox)
                         (< (get-lines-min-distance some-line line) max-distance))
                    ;; (format t "Line ~A inside box~%" some-line)
                    (unless (member some-line lines-list :test #'equal)
                      (push some-line lines-list))))
             (setf lines-list (reverse lines-list))

             (pop lines-list)    ; extract source line
             (dolist (test-line lines-list)
;;               (format t "Test-line = ~A~%" test-line)
               (when
                   (and
                    (or             ; center and one end point of TEST-LINE lie on same
                     (points-on-one-line? ; straight line as LINE.
                      (list (first line) (second line)
                            (first test-line)
                            (center-point test-line)))
                     (points-on-one-line?
                      (list (first line) (second line)
                            (second test-line)
                            (center-point test-line))))
                    ;; distance between lines mustn't be too long.
                    (<= (min-distance line test-line)) (get-line-search-radius))

                 (unless (member line lines-for-merging :test #'equal)
                   (remhash (first line) lines-hash)
                   (remhash (second line) lines-hash)
                   (push line lines-for-merging))

                 (unless (member test-line lines-for-merging :test #'equal)
                   ;; move this to procedure
                   (remhash (first test-line) lines-hash)
                   (remhash (second test-line) lines-hash)
                   ;;(format t "Make line longer. source lines: ~A and ~A ~%" line test-line)
                   (let ((new-line
                          (if (< (third test-line) (third line))
                              (merge-two-lines line test-line)
                              (merge-two-lines test-line line))))
                     ;; debug info here
                     ;;(format t "Result line: ~A ~%~%" new-line)
                     (when (and
                            (not (one-line-lies-on-the-other line test-line))
                            (< 10 (abs (- (third new-line)
                                          (+ (third line)
                                             (third test-line))))))
                       (format t "Error: source lines: ~%~A~%~A~%" line test-line)
                       (format t "Resulting line:~%~A~%~%" new-line))
                     (incf total-lines-merged)
                     (setf (gethash (first new-line) lines-hash) new-line)
                     (setf (gethash (second new-line) lines-hash) new-line))
                   (push test-line lines-for-merging)
                   (return))))
             (setf lines-list nil))))     ; break from dolist
    (format t "Merges: ~A~%" total-lines-merged)
    total-lines-merged))

(defun points-on-one-line? (points-list &key (distance-delta 1.2) (angle-delta 5))
 "Check if POINTS-LIST on one line. DISTANCE-DELTA -- maximum delta between each two
distances, found by Hough transform. ANGLE-DELTA -- angle step for calculating
Hough transform.
Return nil if points not lie on same line,
otherwise return (t angle max-distance-value) -- for debug purposes.

Examples:
 (points-on-one-line? '((20 10) (30 12) (10 10)) :distance-delta 1.2)
 -> (T 94.99999999999994d0 1.1208319687069164d0)
 (points-on-one-line? '((20 10) (30 13) (10 10)) :distance-delta 1.2)
 -> nil
"
  ;; (get-max-delta '(10 11 11 9 10 11.3))
  ;; (get-max-delta '(-19 -29 -9))
  ;; (get-max-delta '(22.118805249290297d0 30.88390212853126d0 13.927284806400378d0))
  (labels ((get-max-delta (values-list)
             (let ((minn 0) delta)
               (loop for i from 0 to (- (length values-list) 2) do
                    (loop for j from (1+ i) to (- (length values-list) 1) do
                         (setf delta (abs (- (abs (nth i values-list))
                                             (abs (nth j values-list)))))
                         (when (< minn delta)
                           (setf minn delta))))
               minn)))
    (let ((min-delta (* 100 distance-delta)) ; set start min distance delta
          cur-delta
          angle)
      (dolist (points-raw (hough-lines-table points-list :angle-delta angle-delta))
        (setf cur-delta (get-max-delta (cadr points-raw)))
        (when (< cur-delta min-delta)
          (setf min-delta cur-delta)
          (setf angle (first points-raw))))
      (if (<= min-delta distance-delta)
          (list t angle min-delta)
          nil))))

(defun line-bounding-box (line &key (margin 20))
  "Get bounding box of LINE with MARGINS on all of sides.
Example:
 (line-bounding-box '((10 5) (30 10)) :margin 2)
 -> ((8 3) (32 12))
"
  (let ((x1 (first (first line)))
        (y1 (second (first line)))
        (x2 (first (second line)))
        (y2 (second (second line))))
    (list
     (list (- (min x1 x2) margin)
           (- (min y1 y2) margin))
     (list (+ margin (max x1 x2))
           (+ margin (max y1 y2))))))


(defun inside-box? (line bounding-box)
  "Test if all line points inside bounding-box.
Example:
 (inside-box? '((10 5) (30 12)) '((8 3) (32 12)))
 -> T"
  (labels ((point-inside-box? (point bounding-box)
             (let ((x1 (first (first bounding-box)))
                   (y1 (second (first bounding-box)))
                   (x2 (first (second bounding-box)))
                   (y2 (second (second bounding-box))))
               (and
                (<= x1 (first point) x2)
                (<= y1 (second point) y2)))))
    (and
     (point-inside-box? (first line) bounding-box)
     (point-inside-box? (second line) bounding-box))))


(defun make-line-longer (pt1 pt2 length)
  "Make line longer.

      x2 + gamma * x2 - x1
 x = -----------------------
            gamma

      y2 + gamma * y2 - y1
 y = -----------------------
            gamma
"
  (if (< 0 length)
      (let* ((x1 (first pt1))
             (y1 (second pt1))
             (x2 (first pt2))
             (y2 (second pt2))
             (gamma (/  (get-points-distance pt1 pt2) length) )
             (pt3 (list
                   (round (/ (- (+ x2 (* gamma x2)) x1) gamma))
                   (round (/ (- (+ y2 (* gamma y2)) y1) gamma)))))
        (make-line pt1 pt3))
      (make-line pt1 pt2)))

;; (make-line-longer '(19 20) '(20 43) 5)
;; (+ (get-points-distance  '(2 6) '(17 6) ) 8)

