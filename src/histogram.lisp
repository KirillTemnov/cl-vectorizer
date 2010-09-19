
(in-package #:cl-vectorizer)

;; Histogram creation and analysis module

(defun make-histograms (points-list)
  "Make x and y histograms from POINTS-LIST.
Return list containing X-HISTOGRAM and Y-HISTOGRAM."
  (let* ((max-x (apply #'max (map 'list #'(lambda (elem) (first elem)) points-list)))
         (max-y (apply #'max (map 'list #'(lambda (elem) (second elem)) points-list)))
         (x-histogram (make-sequence 'vector (1+ max-x) :initial-element 0))
         (y-histogram (make-sequence 'vector (1+ max-y) :initial-element 0)))
  (dolist (point points-list)
      (incf (aref x-histogram (first point)))
      (incf (aref y-histogram (second point))))
    (list x-histogram y-histogram)))

;;(setf ar (make-histograms '((5 3) (10 10))))
;; (setf ar (make-histograms z))
;; (setf x (first ar))
;; (setf y (second ar))

(defun normalize-histogram (histogram)
  "Normalize HISTOGRAM and return RESULT as vector."
;;  (let ((max-value (/ (float (vector-max histogram)) 100)))
  (let ((max-value ( / (length histogram) 100.0)))
    (loop for i from 0 to (1- (length histogram)) do
         (setf (aref histogram i) (/ (float (aref histogram i)) max-value)))
    histogram))

;; todo change name to get-normalised-histograms ?
(defun make-normalised-histograms (points-list)
  "Make X and Y histograms from POINTS-LIST, representing image.
Histograms normalised to *image width and height* respectvely.
Return list with x-histogram and y-histogram in order."
  (if (eq nil points-list)
      '(nil nil)
      (let* ((xy-histograms (make-histograms points-list))
             (x-histogram (normalize-histogram (first xy-histograms)))
             (y-histogram (normalize-histogram (second xy-histograms))))
        (when (get-debug-mode)
          (format t "x-normalized-histogram : ~% ~A~%" x-histogram)
          (format t "y-normalized-histogram : ~% ~A~%" y-histogram))
        (list x-histogram y-histogram))))

(defun find-peaks (histogram  &key
                   (min-peak-value (get-min-histogram-value-of-circle-center))
                   (min-peak-distance 4))
  "Find peaks on HISTOGRAM that higher than MIN-PEAK-VALUE.
If two peaks stand from each other closer than MIN-PEAK-DISTANCE, they
merged into one peak, which placed between them."
  (let ((peaks (make-sequence 'vector (length histogram) :initial-element 0))
        (prev-peak-value 0)
        (prev-peak-distance 0)
        peak)
    (loop for i from 0 to (1- (length histogram)) do
         (setf peak (aref histogram i))
         (when (< min-peak-value peak)
           (if (< prev-peak-distance min-peak-distance)
                 ;; merge peaks
                 (if (< prev-peak-value peak)
                     ;; least of two peaks set to zero, other set to maximum (from 2 values)
                     (progn
                       (setf (aref peaks (- i prev-peak-distance)) 0)
                       (setf (aref peaks i) peak))
                     (progn
                       (setf (aref peaks i) 0)
                       (setf prev-peak-value peak)))
                  (progn
                    (setf (aref peaks i) peak)
                    (setf prev-peak-distance 0)))
                  (setf prev-peak-value peak))
         (incf prev-peak-distance))
    (when (get-debug-mode)
      (format t "FIND-PEAKS (~A %): ~% ~A ~%" min-peak-value peaks))
    peaks))

(defun get-symmetric-index (peaks center-index left-index min-value &key (max-radius-error 3))
  "Get right-side index of from the CENTER-INDEX and LEFT-INDEX to the right using
peaks vector. Right-side index is a peak, which is greater or equal than min-value.
If there aren't any peaks on the estimated distance function returns nil. "
  (let* ((delta (- center-index left-index))
         (ind (- (+ center-index delta) max-radius-error))
         right-index)
    (loop while (and
                 (< ind (length peaks))
                 (< ind (+ center-index delta max-radius-error))) do
         (when (< min-value (aref peaks ind))
           (setf min-value (aref peaks ind))
           (setf right-index ind))
         (incf ind))
    right-index))

(defun add-circle (circle circles &key (max-radius-error 3))
  "Add new CIRCLE to CIRCLES only if in CIRCLES not present another circle with
center equals to CIRCLE's radius and radius equals to CIRCLE's radius + or - MAX-RADIUS-ERROR."
  (when (<  max-radius-error (getf circle :radius))
    (let ((may-add t))
      (dolist (c circles)
        (when (and
               (equal (getf c :center) (getf circle :center))
               (<= (abs (- (getf c :radius) (getf circle :radius))) max-radius-error))
          (setf may-add nil)))
      (when (not (null may-add))
        (push circle circles))))
  circles)

(defun generate-circles (x-cirles y-circles &key (max-radius-error 3))
  "Compare circles radiuses. If the difference between two radiuses is less
Than MAX-RADIUS-ERROR, add new circle to list."
  (let (circles-list)
    (dolist (cx x-cirles)
      (dolist (cy y-circles)
        (when (<= (abs (- (getf cx :radius) (getf cy :radius))) max-radius-error)

          (let ((center-point (list (getf cx :center) (getf cy :center)))
                (circle-radius (floor (max (getf cx :radius) (getf cy :radius))))
                not-save)
;;            (format t "CIRCLES LIST GENERATION:~A ~%" circles-list)
            (dolist (c circles-list)
              (when  (and
                      (< (get-points-distance (getf c :center) center-point) max-radius-error)
                      (<= (abs (- circle-radius (getf c :radius)))))
                (setf not-save t)))

            (unless not-save
              (progn
                ;; (format t "add CIRCLE: ~A~%"
                ;;         (list circle-radius center-point))
              (push (list :radius circle-radius :center center-point :intersect 0)
                    circles-list)))))))
    circles-list))

(defun wave (histogram &key min-radius
             (min-peak-value (get-min-histogram-value-of-circle-edge))
             (max-radius-error 3))
  "Min % of peak, that belongs to a cirle"
  (let* ((big-peaks (find-peaks histogram))
         (peaks (find-peaks histogram :min-peak-value min-peak-value))
         possibly-circles)
         ;; peak-indexes
         ;; (i min-radius))
    ;;(format t "in wave. peaks: ~A~%" peaks)
    ;; (loop while (< i (- (length peaks) min-radius 1)) do
    ;;      (when (< 0 (aref peaks i))
    ;;        (format t "push ~A~%" i)
    ;;        (push i peak-indexes))
    ;;      (incf i))

    ;; (loop for i from min-radius to (- (length peaks) min-radius 1) do
    ;;      (when (< 0 (aref peaks i))
    ;;        (push i peak-indexes)))
    ;; (nreverse peak-indexes)


    (loop for ind from min-radius to (- (length peaks) min-radius 1) do
         (when (< 0 (aref big-peaks ind))                     ;; (ind peak-indexes)
           (let ((l-index (- ind min-radius)) r-index)
             (loop while (< 0 l-index)   do     ; move to begin of histogram
                  (when (< min-peak-value (aref peaks l-index))
                    ;; search simmetric peak
                    (setf r-index (get-symmetric-index peaks ind l-index min-peak-value
                                                       :max-radius-error max-radius-error))
                    (unless (null r-index)
                      (setf possibly-circles
                            (add-circle (list :center ind
                                              :radius (/ (- r-index l-index) 2.0)
                                              :center-value (aref peaks ind)
                                              :left-side-value (aref peaks l-index)
                                              :max-radius-error max-radius-error)
                                        possibly-circles))))
                  (decf l-index)))))
       possibly-circles))

(defun find-possibly-circles (points-hash &key (min-radius 10) (max-radius-error 3) (window-size 100))
  "Find objects like cirlces on image represented by POINTS-LIST (black points)."
  (let ((points-list (hashtable-keys-to-list points-hash))
        all-circles
        final-result)
;;    (dolist (points-list (separate-points-to-grid points-hash window-size))
      (let* ((xy-histograms (make-normalised-histograms points-list))
             (x-histogram (first xy-histograms))
             (y-histogram (second xy-histograms))
             (x-possibly-circles (wave x-histogram
                                       :min-radius min-radius :max-radius-error max-radius-error))
             (y-possibly-circles (wave y-histogram
                                       :min-radius min-radius :max-radius-error max-radius-error))
             (circles (generate-circles x-possibly-circles y-possibly-circles
                                        :max-radius-error max-radius-error)))
        (format t "x-possibly-circles: ~A ~%" x-possibly-circles)
        (format t "y-possibly-circles: ~A ~%" y-possibly-circles)
        (format t "RESULT ~%~A~%" circles)
        (setf all-circles (append all-circles circles)))
;; )


    ;; remove overlapping circles
    (format t "all-circles ~A~%" all-circles)

    ;; (dolist (circle all-circles)
    ;;   (dolist (c all-circles)
    ;;     (unless (equal c circle)
    ;;       (let ((distance (get-points-distance (getf c :center) (getf circle :center)))
    ;;             (c-radius (getf c :radius))
    ;;             (circle-radius (getf circle :radius)))
    ;;         (when (or
    ;;                (< distance (+ c-radius circle-radius))
    ;;                (< (max c-radius circle-radius) (+ distance (min c-radius circle-radius))))
    ;;           (incf (getf circle :intersect)))))))

    ;; for pushing use hough.lisp storing method: (radius (center-x center-y))
    (dolist (circle all-circles)
      (when (< (getf circle :intersect) 3)
        (push (list (getf circle :radius) (getf circle :center)) final-result)))
    (format t "Final-Result: ~A~%" final-result)
    (if (< 100 (length final-result))
           (progn (format t "Error: too many circles ~%"  ) nil)
           final-result)))


;; (make-normalised-histograms '((3 3) (3 4) (3 5) (4 4) (10 10)))
;; (make-histograms z)
;; (defvar ar (make-normalised-histograms z))
;; (setf ar (make-normalised-histograms z))
;; (setf x (first ar))
;; (setf y (second ar))
;; (find-peaks x)
;; (find-peaks x :min-peak-value 5)





