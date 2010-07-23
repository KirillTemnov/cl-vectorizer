;; this module purpose is proceed files with quad trees and find text objects

(in-package #:cl-vectorizer)

(defun make-qt (infile &key (outfile (change-extension infile "png")))
  "Make a quadtree of image (infile) and save it to outfile."
  (let* ((image-path (resize-to-200-dpi infile :dest-filename (get-temp-png-file)))
         (image (load-image image-path))
         (w (png:image-width image))
         (h (png:image-height image))
         (ht (image-to-hashtable image))
         (qt (make-instance 'qtree :img-hash ht :width w :height h)))
    (format t "Tree created successfuly ~%")
    qt))

;; orients: northwest, northeast, southwest, southeast
(defvar +orients+ '(nw ne sw se))
;; use this color, because color constants in packages.lisp differ
(defconstant +white-color+ 0)
(defconstant +black-color+ 1)


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


(defmethod print-qtree-element ((elem qtree-element) stream)
  (with-slots (size level label color) elem
    (print-unreadable-object (elem stream :type t)
      (format stream "size: ~a, label: ~a, level: ~a, color: ~a " size level label color))))

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

(defgeneric add-black-pixel (root x y)
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


(defmethod initialize-instance :after ((qtree qtree) &key img-hash width height)
  (labels ((get-tree-size (value &optional (size 2))
             (cond
               ((> value size)
                (get-tree-size value (* 2 size)))
               (t
                size))))
    (with-slots (image-hash size root-element) qtree
      (setf image-hash img-hash)
      (setf size (get-tree-size (max width height)))
      (setf root-element  (make-instance 'qtree-element :size size :level 1))
      (loop for point being the hash-key of image-hash do
           (add-black-pixel root-element (first point) (second point)))
      (format t "total pixels add: ~a~%" (hash-table-size image-hash)))))

;; (defmethod create-tree (image-hash width height)
;;   (let* ((size (get-tree-size (max width height)))
;;          (qtree-instance (make-instance 'qtree
;;                                         :image-hash image-hash
;;                                         :size size
;;                                         :root-element (make-instance 'qtree-element
;;                                                                      :size size
;;                                                                      :level 1))))
;;     (with-slots (root-element) qtree-instance
;;       (loop for point being the hash-key of image-hash do
;;            (add-black-pixel root-element (first point) (second point))))
;;     (format t "total pixels add: ~a~%" (hash-table-size image-hash))
;;     qtree-instance))

(defun my-make-list (size index value &key initial-element)
  `(,@(make-list index :initial-element initial-element)
    ,value
    ,@(make-list (- size index 1) :initial-element initial-element)))

(defmethod add-black-pixel ((root qtree-element) x y)
  (with-slots (size childs level color) root
    (cond
      ((= 1 size)	; hit the bottom
       (setf color +black-color+))

      (t
       (let ((half-size (/ size 2))
             (index 3)
             new-root)

         (cond
           ((and			; nw - first half
             (< x half-size)
             (< y half-size))
            (setf index 0))

           ((and			; ne - second half
             (>= x half-size)
             (<  y half-size))
            (setf index 1)
            (setf x (- x half-size)))

           ((and			; sw - third half
             (<  x half-size)
             (>= y half-size))
            (setf index 2)
            (setf y (- y half-size)))

           (t				; se fourth half, index already set in let
            (setf x (- x half-size))
            (setf y (- y half-size))))

         (when (null (nth index childs))
           (let ((child (make-instance 'qtree-element
                                       :size half-size
                                       :level (1+ level)
                                       :parent root
                                       :color +white-color+
                                       :orient index)))
             (setf childs (my-make-list 4 index child))))

         (setf new-root (nth index childs))
;;         (print-element new-root t)
         (add-black-pixel new-root  x y))))))




