
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

(defun find-peaks (histogram  &key (min-peak-value 30) (min-peak-distance 4))
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
    peaks))

;; (make-normalised-histograms '((3 3) (3 4) (3 5) (4 4) (10 10)))
;; (make-histograms z)
;; (defvar ar (make-normalised-histograms z))
(setf ar (make-normalised-histograms z))
(setf x (first ar))
(setf y (second ar))
(find-peaks x)
(find-peaks y)





