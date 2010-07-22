
(in-package #:cl-vectorizer)

(defclass svg-object nil
    ((stroke       :initarg :stroke       :initform "black")
     (stroke-width :initarg :stroke-width :initform 1)
     (id           :initarg :id           :initform nil)
     (object-type  :initarg :object-type  :initform nil))
  (:documentation "Common svg object."))

(defgeneric to-svg (object)
  (:documentation "Transform object to svg-format representation"))
;;------------------------------------------------------------------------------
(defclass svg-point (svg-object)
  ((point       :initarg :point :initform '(0 0))
   (object-type :initform "point"))
  (:documentation "Point svg object."))

(defun make-svg-point (point &key (color "black"))
  "Point is just a list of 2 coordinates."
  (make-instance 'svg-point :point point :stroke color :stroke-width 2))

(defmethod to-svg ((object svg-point))
  (let* ((id (cond ((eq nil (slot-value object 'id)) "")
		   (t (format nil "id=\"~a\" " (slot-value object 'id)))))
	 (pt (slot-value object 'point))
	 (x1 (first pt))
	 (y1 (second pt)))
    (format nil "<line ~a x1=\"~a\" y1=\"~a\" x2=\"~a\" y2=\"~a\" stroke=\"~a\" stroke-width=\"~a\"/>~%" id x1 y1 (1+ x1) (1+ y1)  (slot-value object 'stroke) (slot-value object 'stroke-width))))
;;------------------------------------------------------------------------------
(defclass svg-line (svg-object)
  ((point1      :initarg :point1      :initform '(0 0))
   (point2      :initarg :point2      :initform '(1 1))
   (object-type :initform "line"))
  (:documentation "Line svg object."))

(defun make-svg-line (line &key (color "black") )
  "Line is a list of 2 (or more) points.
Format:
'((x1 y1) (x2 y2))
"
  (make-instance 'svg-line :point1 (first line) :point2 (second line) :stroke color))

(defmethod to-svg ((object svg-line))
  (let* ((id (cond ((eq nil (slot-value object 'id)) "")
		   (t (format nil "id=\"~a\" " (slot-value object 'id)))))
	 (p1 (slot-value object 'point1))
	 (p2 (slot-value object 'point2))
	 (x1 (first  p1))
	 (y1 (second p1))
	 (x2 (first  p2))
	 (y2 (second p2)))
    (format nil "<line ~a x1=\"~a\" y1=\"~a\" x2=\"~a\" y2=\"~a\" stroke=\"~a\" stroke-width=\"~a\"/>~%"
	    id x1 y1 x2 y2 (slot-value object 'stroke) (slot-value object 'stroke-width))))

;;------------------------------------------------------------------------------
(defclass svg-circle (svg-object)
  ((center       :initarg :center       :initform '(0 0))
   (radius       :initarg :radius       :initform 1)
   (fill         :initarg :fill         :initform "black")
   (fill-opacity :initarg :fill-opacity :initform 0)
   (object-type  :initform "circle"))
  (:documentation "Circle svg object."))

(defun make-svg-circle (circle &key (color "black") (width 1) (fill "black") (fill-opacity 0))
  "Circle is a list of radius nad center point.
Format:
'(radius (center-x center-y))
"
  (make-instance 'svg-circle
		 :radius       (first circle)
		 :center       (second circle)
		 :stroke       color
		 :stroke-width width
		 :fill         fill
		 :fill-opacity fill-opacity))

(defmethod to-svg ((object svg-circle))
  (let* ((id (cond ((eq nil (slot-value object 'id)) "")
		   (t (format nil "id=\"~a\" " (slot-value object 'id)))))
	 (center (slot-value object 'center))
	 (radius (slot-value object 'radius)))
    (format nil "<circle ~a cx=\"~a\" cy=\"~a\" r=\"~a\" stroke=\"~a\" stroke-width=\"~a\"  fill=\"~a\" fill-opacity=\"~a\"/>~%"
	    id (first center) (second center) radius (slot-value object 'stroke) (slot-value object 'stroke-width) (slot-value object 'fill) (slot-value object 'fill-opacity))))
	       ;;------------------------------------------------------------------------------
(defclass svg-image (svg-object)
  ((left-point   :initarg  :left-point :initform '(0 0))
   (filename     :initarg  :filename   :initform nil)
   (width        :initarg  :width      :initform "100%")
   (height       :initarg  :height     :initform "100%")
   (object-type   :initform "image"))
  (:documentation "Raster image placed on svg."))

(defun make-svg-image (filename &key (left-point '(0 0)) (width "100%") (height "100%"))
  (make-instance 'svg-image
		 :filename filename
		 :left-point left-point
		 :width width
		 :height height))

(defmethod to-svg ((object svg-image))
  (let* ((id (cond ((eq nil (slot-value object 'id)) "")
		   (t (format nil "id=\"~a\" " (slot-value object 'id)))))
	 (left (slot-value object 'left-point))
	 (x (first left))
	 (y (second left)))
    (format nil "<image ~a xlink:href=\"~a\" x=\"~a\" y=\"~a\" width=\"~a\" height=\"~a\"/>~%"
	    id (slot-value object 'filename) x y (slot-value object 'width) (slot-value object 'height))))

;;------------------------------------------------------------------------------

(defun render-svg-file (entities-list width height)
  (concatenate 'string
	       (format nil "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<!DOCTYPE svg>
<svg width=\"~a\" height=\"~a\" version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" >~%~%" width height)

	       (mapconcat #'(lambda (elem) (to-svg elem)) entities-list)

	       "</svg>"))

(defclass svg-manager nil
  ((entities-list
    :initarg :entities-list :initform nil)
   (width
    :initarg :width :initform "100%")
   (height
    :initarg :height :initform "100%"))
  (:documentation "Manager for svg drawing entities."))

;;------------------------------------------------------------------------------
(defgeneric create-svg-manager (width height)
  (:documentation "Wrapper to make-instance function."))

(defgeneric add-entity (manager entity)
  (:documentation "Add new entity to manager."))

(defgeneric reset-manager (manager)
  (:documentation "Reset manager state (and delete all objects) to default."))

(defgeneric flush-manager (manager filename)
  (:documentation "Save all objects to file."))
;;--------------------------------------------------------------------------------
(defmethod create-svg-manager (width height)
  (make-instance 'svg-manager :width width :height height))

(defmethod add-entity ((manager svg-manager) (entity svg-object))
  (cond
    ((eq nil (slot-value manager 'entities-list))
     (setf (slot-value manager 'entities-list) (list entity)))
    (t
     (push entity (slot-value manager 'entities-list)))))

(defmethod reset-manager ((manager svg-manager))
  (setf (slot-value manager 'entities-list) nil))

(defmethod flush-manager ((manager svg-manager) filename)
  (when (not (eq nil (slot-value manager 'entities-list)))
    (write-to-file
     (render-svg-file (slot-value manager 'entities-list)
		      (slot-value manager 'width)
		      (slot-value manager 'height))
		   (namestring (get-out-path filename)))))

;;--------------------------------------------------------------------------------
(defun hashtable-lines-to-svg-manager (ht manager)
  "Save lines from hash keys to svg manager."
  (let (line (lines 0))

    (loop for point being the hash-key of ht do
	 (setf line (gethash point ht))
	 (when (line? line)
	   (incf lines)
	   (add-entity manager (make-svg-line line :color "green"))))

    (when (get-debug-mode)
      (format t "Total ~a lines pushed to svg manager.~%" lines))
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

;; Remove
;; (defun save-svg (manager &key (filename #p"out.svg"))
;;   "Save manager to svg file."
;;   (setf (slot-value manager 'file-to-save) filename)
;;   (flush-manager manager filename))



;; (defvar lines nil)
;; (push (make-svg-line '((5 5) (10 10) 4)) lines)
;; (push (make-svg-line '((0 0) (2 6) 4)) lines)
;; (render-svg-file lines "100%" "100%")

