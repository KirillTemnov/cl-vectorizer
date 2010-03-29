
(in-package #:cl-vectorizer)

(defclass svg-object nil
    ((stroke       :initarg :stroke       :initform "black")
     (stroke-width :initarg :stroke-width :initform "1")
     (id           :initarg :id           :initform nil)
     (object-type  :initarg :object-type  :initform nil))
  (:documentation "Common svg object"))
;;------------------------------------------------------------------------------
(defclass svg-line (svg-object)
  ((point1      :initarg :point1      :initform '(0 0))
   (point2      :initarg :point2      :initform '(1 1))
   (object-type :initform "line"))
  (:documentation "Line svg object"))

(defun make-svg-line (line &key (color "black") )
  (make-instance 'svg-line :point1 (first line) :point2 (second line) :stroke color))
;;------------------------------------------------------------------------------
(defgeneric to-svg (object)
  (:documentation "Transform object to svg-format representation"))

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
    :initarg :height :initform "100%")
   (file-to-save
    :initarg :file-to-save :initform nil))
   (:documentation "Manager for svg drawing entities."))

;;------------------------------------------------------------------------------
(defgeneric create-svg-manager (width height &key filename)
  (:documentation "Wrapper to make-instance function."))

(defgeneric add-entity (manager entity)
  (:documentation "Add new entity to manager."))

(defgeneric reset-manager (manager)
  (:documentation "Reset manager state (and delete all objects) to default. Slot 
file-to-save is not resets!"))

(defgeneric flush-manager (manager)
  (:documentation "Save all objects to file."))
;;--------------------------------------------------------------------------------
(defmethod create-svg-manager (width height &key (filename "out.svg"))
  (make-instance 'svg-manager :file-to-save (get-out-path filename) :width width :height height))

(defmethod add-entity ((manager svg-manager) (entity svg-object))
  (cond
    ((eq nil (slot-value manager 'entities-list))
     (setf (slot-value manager 'entities-list) (list entity)))
    (t
     (push entity (slot-value manager 'entities-list)))))

(defmethod reset-manager ((manager svg-manager))
  (setf (slot-value manager 'entities-list) nil))

(defmethod flush-manager ((manager svg-manager))
  (when (not (eq nil (slot-value manager 'entities-list)))
    (write-to-file 
     (render-svg-file (slot-value manager 'entities-list)
		      (slot-value manager 'width)
		      (slot-value manager 'height))
		   (namestring (slot-value manager 'file-to-save)))))

;;--------------------------------------------------------------------------------
(defun save-hashtable-as-svg (ht width height &key (filename #p"out.svg"))
  (let ((mgr (create-svg-manager  width height :filename filename))
	line
	(lines 0))
    (loop for point being the hash-key of ht do
	 (setf line (gethash point ht))
	 (when (line? line)
;;	   (print-line line)
	   (incf lines)
	   (add-entity mgr (make-svg-line line :color "green"))))
    (format t "Total ~a lines pushed to svg~%" lines)
    (flush-manager mgr)))

;; (defvar lines nil)
;; (push (make-svg-line '((5 5) (10 10) 4)) lines)
;; (push (make-svg-line '((0 0) (2 6) 4)) lines)
;; (render-svg-file lines "100%" "100%") 

