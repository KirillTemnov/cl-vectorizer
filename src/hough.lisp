
(in-package #:cl-vectorizer)

;; Hough transformation module
;; using for search circles
;; todo circles can't be intersect (or set flag)
;; todo set minumal radius too


(defun get-near-points (point points-list max-distance)
  "Get list of points that are at a MAX-DISTANCE or closer to POINT."
  (let (near-points)
    (dolist (pt points-list)
      (let ((distance-to-pt (get-points-distance point pt)))
        (when (>= max-distance distance-to-pt)
          (push pt near-points))))
    near-points))

(defun get-circle-radius-and-center (p1 p2 p3)
  "Get radius and coordinates of center point of circle, that build on points P1, P2 and P3.
Returns list (radius (cx cy))."
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
  (declare (optimize (speed 3) (safety 0)))
  (let* ((x1 (first p1))
         (x2 (first p2))
         (x3 (first p3))
         (y1 (second p1))
         (y2 (second p2))
         (y3 (second p3))
         (divisor (-
                   (* (- x2 x3) (- y1 y2))
                   (* (- x1 x2) (- y2 y3)))))
    (declare (number x1 x2 x3 y1 y2 y3 divisor))

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
          (declare (number summand1 summand2 a b radius))
          (list (round radius) (list (round a) (round b)))))))

(defun can-build-circle? (p1 p2 p3 max-diameter)
  "Test if distance between point pairs (P1 P2) (P2 P3) (P1 P3) less then MAX-DIAMETER."
  (if (< (max (get-points-distance p1 p2)
              (get-points-distance p2 p3)
              (get-points-distance p3 p1))
         max-diameter)
      t nil))

(defun merge-two-circles (circle1 circle2)
  "Merge two circles in one. Circle here is a list: (radius (center-x center-y)).
Returns average circle (average center and average radius)."
  (list (round (avg (first circle1) (first circle2)))
        (list (round (avg (first (second circle1)) (first (second circle2))))
              (round (avg (second (second circle1)) (second (second circle2)))))))

(defun find-similar-circles (circle circles-hash)
  "Find circles, similar to CIRCLE by radius and center, inside CIRCLES-HASH."
  (labels ((similar-circles? (circ1 circ2)
           ;; Check if CIRC1 and CIRC2 have approximately the same
           ;; radius and center point.
           (let ((center-delta 5)       ; max center points distance is 4 points
                 (delta-r 2))   ; radius may vary in 2 points
             (and
              (>= delta-r (- (first circ1) (first circ2)))
              (>= center-delta (get-points-distance (second circ1) (second circ2)))))))
    (loop for cur-circle being the hash-key of circles-hash do
         ;;(format t "cur-circle ~A circle = ~A~%" cur-circle circle)
         (when (and
                (not (equal circle cur-circle))
                (similar-circles? circle cur-circle))
           (let ((pts-total
                   (+ (gethash circle circles-hash 0) (gethash cur-circle circles-hash 0))))
             (when (get-debug-mode)
               (format t "Merge circles. ~%First: ~a~%" circle)
               (format t "Second ~a ~%" cur-circle))

             ;; remove both circles, add merged circle
             (remhash circle circles-hash)
             (remhash cur-circle circles-hash)
             (setf (gethash (merge-two-circles circle cur-circle) circles-hash) pts-total))))
    circles-hash))


(defun merge-hashed-circles (circles-hash)
  "Merge of several hashes circles in one.
As circles may have offsets during the vectorization process, inside CIRCLES-HASH
may real circles consists of several circles with small offset of center point ( < 3 pt)
and small offset from circle radius. This method merge such circles
 *putting all points in one resulting circle*.
Resulting circle and its points writes to CIRCLES-HASH, other circles removed from it."
  (loop for circle being the hash-key of circles-hash do
       (find-similar-circles circle circles-hash)))


(defun separate-points-to-grid (points-hash grid-size)
  "Separate points in POINTS-HASH to 'cells' of grid.
 Function returns list of list of points.
 Each cell have definite place on 'canvas' where points located,
 thereby canvas separates by a grid. Cells of grid are squares and
 they overlap each other on a half width, so,
 returned result contains dublicates of most of the points."
  (let ((points-list  (hashtable-keys-to-list points-hash))
        (max-xy (get-max-coordinates points-hash))
        (grid-size/2 (round (/ grid-size 2)))
        points-grid-list)

    (loop for x from 0 to (first max-xy) by grid-size/2 do
         (loop for y from 0 to (second max-xy) by grid-size/2 do
              (let ((x2 (+ grid-size x))
                    (y2 (+ grid-size y))
                    current-list)
                (dolist (point points-list)
                  (when (and
                         (<= x (first point)  x2)
                         (<= y (second point) y2))
                    (push point current-list)))
                (push current-list points-grid-list))))
    points-grid-list))

(defun find-circles (points-hash possibly-circles-list
                     &key (max-distance 100) (max-radius-error 3))
  "Find circles, preselected by histogram analisys"
  (let ((circles-hash (make-hash-table :test #'equal :size 64))
        suitable-points)
    (dolist (circle possibly-circles-list)
      (let ((circle-center (second circle))
            (circle-radius (first circle))
            circle-params)
        ;; select points, that indent from circle center to
        ;; circle-radius value +/- max-radius-error.
        (loop for point being the hash-key of points-hash do
             (when (< (abs (- circle-radius (get-points-distance circle-center point)))
                      max-radius-error)
               (push point suitable-points))
             (setf (gethash circle circles-hash) 1))

        ;; (format t "circle: ~A~%" circle)
        ;; (format t "Point found: ~A~%" (length suitable-points))
        (if (< (length suitable-points) 100) ; remove evident outsiders
            (remhash circle circles-hash)
          ;; eval Hough transform on this points
          ;;        (block nested-loops
          (dolist (p1 suitable-points)
            (when (< 10000 (gethash circle circles-hash))
              (return))
            (dolist (p2 suitable-points)
              (when (< 10000 (gethash circle circles-hash))
                (return))
              (dolist (p3 suitable-points)
                (when (and                  ; unless .. or .. ?
                       (not (equal p1 p2))
                       (not (equal p2 p3))
                       (not (equal p1 p3)))
                  (setf circle-params (get-circle-radius-and-center p1 p2 p3))
                  (when (and circle-params
                             (< (abs (- circle-radius (first circle-params))) max-radius-error))
                    ;; check center!

                    (incf (gethash circle circles-hash))))))))))

    (format t "circles: ~A~%"      (hash-table-count circles-hash))
    (print-hash circles-hash)
    circles-hash))


(defun find-circles2 (points-hash max-distance min-radius)
  "Find circles by Hough transformation."
  ;;  (declare (optimize (speed 3)))

  (let* ((points-list-grid (separate-points-to-grid points-hash max-distance))
         (circles-hash (make-hash-table :test #'equal :size 2048))
         circle-params
         (index 0))

    (dolist (points-list points-list-grid)
      (incf index)
      (dolist (p1 points-list)
        (dolist (p2 points-list)
          (dolist (p3 points-list)
            (when (can-build-circle? p1 p2 p3 max-distance)
              (setf circle-params (get-circle-radius-and-center p1 p2 p3))
              (when (and
                     circle-params
                     (> max-distance (first circle-params))
                     (< min-radius (first circle-params)))

                (if (gethash circle-params circles-hash)
                    (incf (gethash circle-params circles-hash))
                    (setf (gethash circle-params circles-hash) 1))))))))

    (maphash #'(lambda (circle points)
                 (when (< points 320)   ; todo 28.09.2010 experimental value, move to settings
                   (remhash circle circles-hash)))
             circles-hash)

    ;; debug info
    (format t "circles: ~A~%~%"      (hash-table-count circles-hash))
    (print-hash circles-hash)
    (merge-hashed-circles circles-hash)
    (format t "circles 2: ~A~%"      (hash-table-count circles-hash))

    circles-hash))


(defun get-max-angle (radius)
  "Get maximum angle distance for specified RADIUS."
  (dolist (rad-condition (get-angles-step-for-circle))
    (when (<= radius (first rad-condition))
      (return-from get-max-angle (second rad-condition)))) 0)

;; (defun remove-overlaping-circles (circles-hash)
;;   "Remove overlapping circles from CIRCLES-HASH."
;;   (let* ((circles
;;          (loop for circle being the hash-key of circles-hash collect circle))
;;         (cur-circle (pop circles)))
;;     (
;;     (dolist (circle circles)
;;       (
  ;; (loop for circle being the hash-key of circles-hash
  ;;    using (hash-value points)
  ;;    do
  ;;      (
  ;;      ))



