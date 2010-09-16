
(in-package #cl-vectorizer)

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

(defun make-normalised-histograms (points-list)
  "Make X and Y histograms from POINTS-LIST, representing image.
Histograms normalised to *image width and height* respectvely.
Return list with x-histogram and y-histogram in order."
  (let* ((xy-histograms (make-histograms points-list))
        (x-histogram (normalize-histogram (first xy-histograms)))
        (y-histogram (normalize-histogram (second xy-histograms))))
    (list x-histogram y-histogram)))

(defun find-peaks (histogram  &key (min-peak-value 30) (offset 0)
                   (min-peak-distance 4))
  "Find peaks on HISTOGRAM that higher than MIN-PEAK-VALUE.
If two peaks stand from each other closer than MIN-PEAK-DISTANCE, they
merged into one peak, which placed between them. OFFSET parameter uses for
skip several first and last points on histogram. "
  (let ((peaks (make-sequence 'vector (length histogram) :initial-element 0))
        (prev-peak-value 0)
        (prev-peak-distance 0)
        peak)
    (loop for i from offset to (- (length histogram) offset 1) do
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
    peaks))


(defun find-possibly-circles (points-list &key (min-radius 10))
  "Find objects like cirlces on image represented by POINTS-LIST (black points)."
  (labels ((get-symmetric-index (peaks center-index left-index min-value)
             "Get right-side index of from the CENTER-INDEX and LEFT-INDEX to the right using
peaks vector. Right-side index is a peak, which is greater or equal than min-value.
If there aren't any peaks on the estimated distance function returns nil. "
             (let* ((delta (- center-index left-index))
                    (max-radius-error 3)       ; error in determining the radius
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

           (add-circle (circle circles &key (max-radius-error 3))
             "Add new CIRCLE to CIRCLES only if in CIRCLES not present another circle with
center equals to CIRCLE's radius and radius equals to CIRCLE's radius + or - MAX-RADIUS-ERROR."
             (let ((may-add t))
               (dolist (c circles)
                 (when (and
                        (= (getf c :center) (getf circle :center))
                        (<= (abs (- (getf c :radius) (getf circle :radius))) max-radius-error))
                   (setf may-add nil)))
               (when (not (null may-add))
                 (push circle circles)))
             circles)

           (generate-circles (x-cirles y-circles &key (max-radius-error 3))
             "Compare circles radiuses. If the difference between two radiuses is less
Than MAX-RADIUS-ERROR, add new circle to list."
             (let (circles-list)
               (dolist (cx x-cirles)
                 (dolist (cy y-circles)
                   (when (<= (abs (- (getf cx :radius) (getf cy :radius))) max-radius-error)
                     (push (list (list (getf cx :center) (getf cy :center))
                                 (floor (max (getf cx :radius) (getf cy :radius))))
                           circles-list))))
               circles-list))

           (wave (histogram &key (min-peak-value 7)) ; min % of peak, that belongs to a cirle
             (let* ((peaks (find-peaks histogram))
                    peak-indexes
                    possibly-circles)
               (loop for i from min-radius to (- (length peaks) min-radius 1) do
                    (when (< 0 (aref peaks i))
                      (push i peak-indexes)))
               (nreverse peak-indexes)
               (setf peaks (find-peaks histogram :min-peak-value min-peak-value))

               (dolist (ind peak-indexes)
                 (let ((l-index (- ind min-radius)) r-index)
                   (loop while (< 0 l-index)   do     ; move to begin of histogram
                        (when (< min-peak-value (aref peaks l-index))
                          ;; search simmetric peak
                          (setf r-index (get-symmetric-index peaks ind l-index min-peak-value))
                          (unless (null r-index)
                            (setf possibly-circles
                                  (add-circle (list :center ind
                                                    :radius (/ (- r-index l-index) 2.0)
                                                    :center-value (aref peaks ind)
                                                    :left-side-value (aref peaks l-index))
                                              possibly-circles))))
                        (decf l-index))))
               possibly-circles)))

    (let* ((xy-histograms (make-normalised-histograms points-list))
           (x-histogram (first xy-histograms))
           (y-histogram (second xy-histograms))
           (x-possibly-circles (wave x-histogram))
           (y-possibly-circles (wave y-histogram)))
      (format t "x-possibly-circles: ~A ~%" x-possibly-circles)
      (format t "y-possibly-circles: ~A ~%" y-possibly-circles)
      (format t "RESULT ~%~A~%" (generate-circles x-possibly-circles y-possibly-circles)))))

;; (make-normalised-histograms '((3 3) (3 4) (3 5) (4 4) (10 10)))
;; (make-histograms z)
;; (defvar ar (make-normalised-histograms z))
(setf ar (make-normalised-histograms z))
(setf x (first ar))
(setf y (second ar))
(find-peaks x)
(find-peaks x :min-peak-value 5)





