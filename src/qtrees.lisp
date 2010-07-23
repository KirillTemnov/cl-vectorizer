;; this module purpose is proceed files with quad trees and find text objects

(in-package #:cl-vectorizer)

(defun make-qt (infile &key (outfile (change-extension infile "png")))
  "Make a quadtree of image (infile) and save it to outfile."
  (let* ((image-path (resize-to-200-dpi infile :dest-filename (get-temp-png-file)))
         (image (load-image image-path))
         (w (png:image-width image))
         (h (png:image-height image))
         (ht (image-to-hashtable image))
         (qt (create-tree ht w h)))
    (format t "Tree created successfuly ~%")
    qt))

;; orients: northwest, northeast, southwest, southeast
(defconstant +orients+ '(nw ne sw se))
;; use this color, because color constants in packages.lisp differ
(defconstant +white-color+ 0)
(defconstant +black-color+ 1)


;; internal clases and functions
;; (defclass pixel nil
;;   ((x  :initarg :x :initform 0)
;;    (y  :initarg :y :initform 0)
;;    (color  :initarg :color :initform nil))
;;   (:documentation "Pixel of image with color.")

;;--------------------------------------------------------------------------------
(defclass qtree-element nil
  ((size     :initarg :size)
   (level    :initarg :level)
   (label    :initarg :label :initform 0)
   (childs   :initarg :childs :initform '(nil nil nil nil))
   (parent   :initarg :parent :initform nil)
   (color    :initarg :color :initform nil)
   (orient   :initarg :orient :initform (first +orients+))
   (path     :initarg :path :initform nil)
   (density  :initarg :density :initform nil))
  (:documentation "Quadtree base element."))
;;--------------------------------------------------------------------------------
(defgeneric print-element (elem)
  (:documentation "Print qtree-element slots."))

(defmethod print-element ((elem qtree-element))
  (with-slots (size level label parent childs) elem
    (format t "[qtree element] ~a size: ~a  level: ~a  label: ~a   parent: ~a childs: ~a~%"
            elem size level label parent childs)))



;;--------------------------------------------------------------------------------
(defclass qtree nil
  ((image-hash    :initarg :image-hash :initform nil)
   (size          :initarg :size :initform nil)
   (root-element  :initarg :root-element :initform nil))
  (:documentation "Quadtree class."))

;;--------------------------------------------------------------------------------
(defgeneric create-tree (image-hash width height)
  (:documentation "Create instance of qtree class."))

(defgeneric dump-tree (tree filename)
  (:documentation "Dump tree to a file."))

(defgeneric get-leaf (tree path)
  (:documentation "Get tree element by path."))

(defgeneric add-black-pixel (root pixel)
  (:documentation "Add black pixel to quadtree."))

;;--------------------------------------------------------------------------------
(defun get-tree-size (value &optional (size 2)) ; todo move to flet ?
  "Get minimum size, that greater or equal then value.
Size repersented py power of 2.
Example:
 (get-tree-size 129)
256
"
  (cond
    ((> value size)
     (get-tree-size value (* 2 size)))
    (t
     size)))


(defmethod create-tree (image-hash width height)
  (let* ((size (get-tree-size (max width height)))
         (qtree-instance (make-instance 'qtree
                                        :image-hash image-hash
                                        :size size
                                        :root-element (make-instance 'qtree-element
                                                                     :size size
                                                                     :level 1))))
    (loop for point being the hash-key of image-hash do
         (add-black-pixel (slot-value qtree-instance 'root-element) point))

    qtree-instance))

(defmethod add-black-pixel ((root qtree-element) pixel)
  (with-slots (size childs level color) root
    (cond
      ((= 1 size)	; hit the bottom
       (format t "Add pixel: ~a ~%" pixel)
       (setf color +black-color+))

      (t
       ;; (print-element root)
       ;; (format t "Root childs: ~a~%" childs)

       (let ((x (first  pixel))
             (y (second pixel))
             (half-size (/ size 2))
             (index 3)
             new-root)

         (format t "Add new pixel, level ~a (~a ~a) size: ~a~%" level x y size)
         (cond
           ((and			; nw - first half
             (< x half-size)
             (< y half-size))
            (setf index 0))

           ((and			; ne - second half
             (>= x half-size)
             (<  y half-size))
            (setf index 1)
            (setf x (- half-size x)))

           ((and			; sw - third half
             (<  x half-size)
             (>= y half-size))
            (setf index 2)
            (setf y (- half-size y)))

           (t				; se fourth half, index already set in let
            (setf x (- half-size x))
            (setf y (- half-size y))))

         (when (eq nil (nth index childs))
           (let ((child (make-instance 'qtree-element
                                       :size half-size
                                       :level (1+ level)
                                       :parent root
                                       :color +white-color+
                                       :orient index)))
             (format t "create child!~%"  )
             (setf (nth index childs) child)))


         (setf new-root (first childs))
         ;; (print-element new-root)
         (format t "~%~%"  )
         (when (equal root new-root)
           (error "WTF?!! root = new-root"))
         (format t "----------------------------------------~%"  )
         (sleep 1)
         (add-black-pixel new-root (list x y)))))))




