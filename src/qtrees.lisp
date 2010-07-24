;; this module purpose is proceed files with quad trees and find text objects

(in-package #:cl-vectorizer)

(defun make-qt (infile &key (outfile (change-extension infile "png")))
  "Make a quadtree of image (infile) and save it to outfile."
  (declare (ignore outfile))
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

(defun update-list (list index new-value)
  (let ((new-list (copy-list list)))
    (setf (nth index new-list) new-value)
    new-list))

;;--------------------------------------------------------------------------------
(defclass qtree-node nil
  ((size     :initarg :size)
   (level    :initarg :level)
   (label    :initarg :label :initform 0)
   (childs   :initarg :childs :initform '(nil nil nil nil))
   (parent   :initarg :parent :initform nil)
   (color    :initarg :color :initform nil)
   (orient   :initarg :orient :initform (first +orients+))
   (path     :initarg :path :initform nil)
   (density  :initarg :density :initform 0))
  (:documentation "Quadtree base node."))
;;--------------------------------------------------------------------------------
(defgeneric print-qtree-node (node stream)
  (:documentation "Print qtree-node slots."))

(defgeneric offset (node root-size)
  (:documentation "Get node offset from right top corner of image." ))


(defgeneric recalc-colors (node)
  (:documentation "Recalculate color value for node.  Color of node is a sum of all
undelying nodes. Color of node with size 1 is +white-color+ or +black-color+."))

(defgeneric map-tree (root eval-func)
  (:documentation "Visit all nodes of tree from root and below, evaluate eval-func on each
  of nodes."))


(defmethod print-qtree-node ((node qtree-node) stream)
  (with-slots (size level label color density) node
    (print-unreadable-object (node stream :type t)
      (format stream "size: ~a, label: ~a, level: ~a, color: ~a, density: ~a~%"
              size level label color density))))

(defmethod offset((node qtree-node) root-size)
  (with-slots (path) node
    (let ((x 0) (y 0))
      (dolist (i path)
        (setf x (+ x (* (logand 1 i) (/ root-size 2))))
        (setf y (+ y (* (ash (logand 2 i) -1) (/ root-size 2))))
        (setf root-size (/ root-size 2)))
      (list x y))))

(defmethod recalc-colors ((node qtree-node))
  (with-slots (size childs color density) node
    (cond
      ((= 1 size) (return-from recalc-colors color))
      ((equal '(nil nil nil nil) childs)
       (setf color +white-color+))
      (t
       (setf color (apply #'+ (map 'list
                                   #'(lambda (child)
                                       (cond
                                         ((null child) +white-color+)
                                         (t (recalc-colors child))))
                                   childs)))))
    (setf density (/ (float color) (* size size)))
    color))

(defmethod map-tree ((root qtree-node) eval-func)
  (funcall eval-func root)
  (with-slots (childs size) root
    (format t "map-tree childs= (~a)~%" childs)
    (map 'list #'(lambda (child)
                   (cond
                     ((null child) nil)
                     (t
                      (map-tree child eval-func)))) childs)))

;;--------------------------------------------------------------------------------
(defclass qtree nil
  ((image-hash    :initarg :image-hash :initform nil)
   (size          :initarg :size :initform nil)
   (root-node     :initarg :root-node :initform nil))
  (:documentation "Quadtree class."))

;;--------------------------------------------------------------------------------
(defgeneric create-tree (image-hash width height)
  (:documentation "Create instance of qtree class."))

(defgeneric dump-tree (root hash)
  (:documentation "Dump tree to a hash table."))

(defgeneric get-leaf (root path)
  (:documentation "Get tree node by path. Path is a list of numbers 0, 1, 2 and 3."))

(defgeneric add-black-pixel (root x y)
  (:documentation "Add black pixel to quadtree."))

(defgeneric print-tree (root)
  (:documentation "Print tree nodes."))

(defmethod print-tree ((tree qtree))
  (with-slots (root-node) tree
    (print-tree root-node)))

(defmethod print-tree ((root qtree-node))
  (with-slots (childs size) root
    (format t "node~a, childs = ~a~%" root childs)
    (when (not (null (nth 0 childs)))
      (print-tree (nth 0 childs)))
    (when (not (null (nth 1 childs)))
      (print-tree (nth 1 childs)))
    (when (not (null (nth 2 childs)))
      (print-tree (nth 2 childs)))
    (when (not (null (nth 3 childs)))
      (print-tree (nth 3 childs)))))

;;--------------------------------------------------------------------------------
(defmethod initialize-instance :after ((qtree qtree) &key img-hash width height)
  (labels ((get-tree-size (value &optional (size 2))
             (cond
               ((> value size)
                (get-tree-size value (* 2 size)))
               (t
                size))))
    (with-slots (image-hash size root-node) qtree
      (setf image-hash img-hash)
      (setf size (get-tree-size (max width height)))
      (setf root-node  (make-instance 'qtree-node :size size :level 1))
      (loop for point being the hash-key of image-hash do
;;           (format t "path = [ " )
           (add-black-pixel root-node (first point) (second point)))
      (format t "total pixels add: ~a~%" (hash-table-count image-hash)))))

(defmethod get-leaf ((root qtree-node) path)
  (with-slots (childs) root
    (cond
      ((or (null root)
           (null path)) root)
      (t
       (get-leaf (nth (first path) root)  (last path))))))

(defmethod add-black-pixel ((root qtree-node) x y)
  (with-slots (size childs level color) root
    (cond
      ((= 1 size)	; hit the bottom
       (progn
;;         (format t "]~%++++++++++++++++++++++++++++++++++++++++~%"  )
         (setf color +black-color+)))

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
           (let ((child (make-instance 'qtree-node
                                       :size half-size
                                       :level (1+ level)
                                       :parent root
                                       :color +white-color+
                                       :orient index)))
             (setf childs (update-list childs index child))))

         (setf new-root (nth index childs))
         (when (equal root new-root) (error "WTF!!!!?"))
;;         (format t "~a, " index)
         (add-black-pixel new-root  x y))))))




