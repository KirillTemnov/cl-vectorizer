;; this module purpose is proceed files with quad trees and find text objects

(in-package #:cl-vectorizer)

(defun make-qt (infile &key (outfile (change-extension infile "png")))
  "Make a quadtree of image (infile) and save it to outfile."
  (let* ((image-path (resize-to-200-dpi infile :dest-filename (get-temp-png-file)))
	 (image (load-image image-path))
	 (w (png:image-width image))
	 (h (png:image-height image))
	 (ht (image-to-hashtable image)))
))

;; orients: northwest, northeast, southwest, southeast
(defconstant +orients+ '(nw ne sw se))
;; use this color, because color constants in packages.lisp differ
(defconstant +white-color+ 0)
(defconstant +black-color+ 0)


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
   (childs   :initarg :childs :initform nil)
   (parent   :initarg :parent :initform nil)
   (color    :initarg :color :initform nil)
   (orient   :initarg :orient :initform (first +orients+))
   (path     :initarg :path :initform nil)
   (density  :initarg :density :initform nil))
  (:documentation "Quadtree base element."))

;;--------------------------------------------------------------------------------
(defclass qtree nil
  ((image-hash    :initarg :image-hash :initform nil)
   (size          :initarg :size :initform nil)
   (root-element  :initarg :root-element :initform nil))
   (:documentation "Quadtree class."))

;;--------------------------------------------------------------------------------
(defgeneric create-tree (image-hash width height)
  (:documentation "Create instance of qtree class."))

(defgeneric dump-tree (tree, filename)
  (:documentation "Dump tree to a file."))

(defgeneric get-leaf (tree, path)
  (:documentation "Get tree element by path."))

(defgeneric add-pixel (tree, root, pixel)
  (:documentation "Add pixel to tree."))

(max 3 4)
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
        ;;code here
        )

