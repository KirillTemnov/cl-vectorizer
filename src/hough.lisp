
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
;  (declare (optimize (speed 3) (safety 0))
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
  (list (avg (first circle1) (first circle2))
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
As circles may have offsets during the vectorization process, inside CIRCLES-HASH
may real circles consists of several circles with small offset of center point ( < 3 pt)
and small offset from circle radius. This method merge such circles
 *putting all points in one resulting circle*.
Resulting circle and its points writes to CIRCLES-HASH, other circles removed from it."
  (loop for circle being the hash-key of circles-hash do
       (find-similar-circles circle circles-hash)))

(defun find-circles (points-hash max-distance min-radius)
  "Find circles by Hough transformation method."

  (let ((circles-hash (make-hash-table :test #'equal))
        (points-list (hashtable-keys-to-list points-hash))
        circle-params
        near-points-list
        p1 (i 0))

    (labels ((analyse-circle (circle points min-radius)
               (let* ((radius (first circle))
                      (center (second circle)))
                 (declare (ignore center))
                 ;;      angles angle-step prev-angle (steps-total 0))
                 ;; (dolist (p points)
                 ;;   (push (round (get-tilt-angle (list center p))) angles))
                 (when (and (<= min-radius radius) (> 13 (length points)))
                   (remhash circle circles-hash))

                     ;; search arcs HERE
                   )))

      (loop while (< 0 (length points-list)) do
           (progn
             (incf i)
             (when (get-debug-mode)
               (format t "tick ~a. List length = ~a...~%" i (length points-list)))
             (setf p1 (pop points-list))
             (setf near-points-list (get-near-points p1 points-list max-distance))
             (format t "point ~A proceed. Near points: ~A  ~%"
                     (length points-list) (length near-points-list))

             (dolist (p2 near-points-list)
               (dolist (p3 near-points-list)
                 (when (can-build-circle? p1 p2 p3 max-distance)
                   (setf circle-params (get-circle-radius-and-center p1 p2 p3))
                   (when circle-params
                     ;;   (format t "Circle params: ~a~%" circle-params)
                     (let ((hash-val (gethash circle-params circles-hash)))
                       (setf hash-val (push-to-list-if-not-present hash-val p1 p2 p3))
                       (setf (gethash circle-params circles-hash) hash-val))))))
             ;;    (format t "Circle params: ~a~%" circles-hash)))
             (merge-hashed-circles circles-hash)


             (loop for circle being the hash-key of circles-hash
                using (hash-value points)
                do
                  (analyse-circle circle points min-radius))

             ;; (let ((lst (gethash circle circles-hash)))
             ;;   (when (> 10 (length lst)) ; change this to more complex condition
             ;;     (remhash circle circles-hash))))
             ))

      circles-hash)))

(defun separate-points-to-grid (points-hash grid-size)
  "Separate points in POINTS-HASH to 'cells' of grid.
 Function returns list of list of points.
 Each cell have definite place on 'canvas' where points located,
 thereby canvas separates by a grid. Cells of grid are squares and
 they overlap each other on a half width, so,
 returned result contains dublicates of most of the points."
  (let ((points-list  (hashtable-keys-to-list points-hash))
;; (sort (hashtable-keys-to-list points-hash)
;;                            #'(lambda (point1 point2)
;;                                (if (<= (first point1) (first point2))
;;                                    t nil))))
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


(defun find-circles2 (points-hash max-distance min-radius)
  "Find circles by Hough transformation."
  (declare (optimize (speed 3)))

  (let* ((points-list-grid (separate-points-to-grid points-hash max-distance))

         (circles-hash (make-hash-table :test #'equal :size 2048))
         circle-params
         (index 0))
;;        (index (length points-list-grid)))

    (dolist (points-list points-list-grid)
      (format t "Tick = ~A~%" index)
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

    (loop for circle being the hash-key of circles-hash
       using (hash-value times) do
         (unless (funcall (get-circles-tolerance-func) circle times); (get-circles-tolerance))
           (remhash circle circles-hash)))
    (print-hash circles-hash)
    (format t "circles: ~A~%"      (hash-table-count circles-hash))

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


;; (defun analyse-circles (circles-hash)
;;   "Analysing each circle if it's a circle, an arc or just a points set.
;; Return 2 hash tables in list: arcs and circles."
;;   (let ((new-hash-arcs    (make-hash-table :test #'equal))
;;         (new-hash-circles (make-hash-table :test #'equal)))
;;     (declare (ignore new-hash-arcs))
;;     (labels ((analyse-circle (circle points)
;;                (let* ((radius (first circle))
;;                       (center (second circle))
;;                       angles angle-step prev-angle (steps-total 0))
;;                  (dolist (p points)
;;                    (push (round (get-tilt-angle (list center p))) angles))
;;                  (when (< 10 (length angles))
;;                    ;; (format t "Angles before sort ~a~%" angles)
;;                    ;; (setf angles (sort angles #'<))
;;                    ;; (format t "Angles after sort ~a~%" angles)

;;                    ;; ;; set angle-step
;;                    ;; (cond
;;                    ;;   ((> 10 radius)
;;                    ;;    (setf angle-step 15))
;;                    ;;   ((> 20 radius)
;;                    ;;    (setf angle-step 10))
;;                    ;;   (t
;;                    ;;    (setf angle-step 5)))

;;                    ;; (setf prev-angle (first angles))
;;                    ;; (dolist (angle (rest angles))
;;                    ;;   (if (> angle-step (- angle prev-angle))
;;                    ;;       (progn
;;                    ;;         (incf steps-total)
;;                    ;;         (setf prev-angle angle))
;;                    ;;       (return)))

;; ;;                   (when (<= (/ 360 angle-step) steps-total)
;;                    (when (<= 19 (length angles))
;;                      (setf (gethash circle new-hash-circles) points))

;;                      ;; search arcs HERE
;;                      ;; (let ((start-angle (first angles))
;;                      ;;     arc)
;;                      ;;   (loop for i from 1 to (length angles))
;;                      ;;   (if (aref

;;                      (when (get-debug-mode)
;;                        (format t "Condition = ~a ~%" (get-max-angle radius))
;;                        (format t "Circle points = ~a ~%" points)
;;                        (format t "Circle: ~a      angles: ~a~%" circle angles))))))

;;       (maphash #'(lambda (circle points)
;;                    (analyse-circle circle points)) circles-hash))
;;     new-hash-circles))

