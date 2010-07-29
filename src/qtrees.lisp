;; this module purpose is proceed files with quad trees and find text objects

(in-package #:cl-vectorizer)

;; orients: northwest, northeast, southwest, southeast
;;(defvar +orients+ '(nw ne sw se))
;; use this color, because color constants in packages.lisp differ
(defconstant +white-color+ 0)
(defconstant +black-color+ 1)

(defun update-list (list index new-value)
  "Update one element of list (with INDEX) with NEW-VALUE. Create a copy of LIST and
then update it."
  (let ((new-list (copy-list list)))
    (setf (nth index new-list) new-value)
    new-list))

;;------------------------------------------------------------
;; class definitions
;;------------------------------------------------------------
(defclass qtree-node nil
  ((size     :reader   node-size    :initarg :size)
   (level    :reader   node-level   :initarg :level)
   (label    :accessor node-label   :initarg :label   :initform 0)
   (childs   :accessor node-childs  :initarg :childs  :initform (copy-list '(nil nil nil nil)))
   (parent   :reader   node-parent  :initarg :parent  :initform nil)
   (color    :accessor node-color   :initarg :color   :initform 0)
   (orient   :reader   node-orient  :initarg :orient  :initform nil)
   (density  :accessor node-density :initarg :density :initform 0))
  (:documentation "Quadtree base node."))

(defclass qtree nil
  ((image-hash    :accessor tree-image-hash :initarg :image-hash :initform nil)
   (size          :accessor tree-size       :initarg :size :initform nil)
   (root-node     :accessor tree-root-node  :initarg :root-node :initform nil))
  (:documentation "Quadtree class."))
;;------------------------------------------------------------
;; initialize-instance section
;;------------------------------------------------------------
(defmethod initialize-instance :after ((qtree qtree) &key img-hash width height)
  (labels ((get-tree-size (value &optional (size 2))
             (if (> value size)
                 (get-tree-size value (* 2 size))
                 size)))
      (setf (tree-image-hash qtree) img-hash)
      (setf (tree-size qtree) (get-tree-size (max width height)))
      (setf (tree-root-node qtree)  (make-instance 'qtree-node :size (tree-size qtree) :level 1))
      (loop for point being the hash-key of (tree-image-hash qtree) do
           (add-black-pixel (tree-root-node qtree) (first point) (second point) :path nil))
      (format t "total pixels add: ~a~%" (hash-table-count (tree-image-hash qtree)))))
;;------------------------------------------------------------
;; generics section
;;------------------------------------------------------------
(defgeneric print-qtree-node (node stream)
  (:documentation "Print qtree-node slots."))

(defgeneric recalc-colors (node)
  (:documentation "Recalculate color value for node.  Color of node is a sum of all
undelying nodes. Color of node with size 1 is +white-color+ or +black-color+."))

(defgeneric map-tree (tree eval-func  &key path state)
  (:documentation "Visit all nodes of tree from root and below, evaluate eval-func on each
  of nodes."))

(defgeneric dump-tree (tree hash)
  (:documentation "Dump tree to a hash table."))

(defgeneric print-tree (tree)
  (:documentation "Print tree nodes."))

;; (defgeneric get-leaf (tree path)
;;   (:documentation "Get tree node by path. Path is a list of numbers 0, 1, 2 and 3."))

(defgeneric add-black-pixel (node x y &key path)
  (:documentation "Add black pixel to quadtree."))

(defgeneric label-neib (tree cond)
  (:documentation "Recursive label TREE nodes answer the condition, implemented in COND
  function."))
;;------------------------------------------------------------
;; methods section
;;------------------------------------------------------------
(defmethod print-qtree-node ((node qtree-node) stream)
    (print-unreadable-object (node stream :type t)
      (format stream "size: ~a, label: ~a, level: ~a, color: ~a, density: ~a~%"
              (node-size node) (node-level node) (node-label node) (node-color node) (node-density node))))

(defmethod recalc-colors ((tree qtree-node))
    (recalc-colors (tree-root-node tree)))

(defmethod recalc-colors ((node qtree-node))
  (cond
    ((= 1 (node-size node))
     (return-from recalc-colors (node-color node)))

    ((equal '(nil nil nil nil) (node-childs node))
     (setf (node-color node) +white-color+))

    (t
     (setf (node-color node)
           (apply #'+ (map 'list
                           #'(lambda (child)
                               (if (null child)
                                   +white-color+
                                   (recalc-colors child)))
                           (node-childs node))))
     (setf (node-density node) (/ (float (node-color node)) (expt (node-size node) 2)))
     (node-color node))))

(defmethod map-tree ((tree qtree)  eval-func &key (path nil) (state nil))
  (dolist (child (node-childs (tree-root-node tree)))            ; map root childs, this place
    (when (not (null child))        ; is good for parallel computing
      (map-tree child eval-func :path path :state state))))

(defmethod map-tree ((node qtree-node)  eval-func &key path state)
  (funcall eval-func node :path path :state state)
    (dolist (child (node-childs node))
      (when (not (null child))
        (map-tree child eval-func :path (cons (node-orient node) path) :state state))))

;; (defmethod dump-tree

(defmethod print-tree ((tree qtree))
    (print-tree (tree-root-node tree)))

(defmethod print-tree ((node qtree-node))
    (format t "node~a, childs = ~a~%" node (node-childs node))
    (dolist (child (node-childs node))
      (when (not (null child))
        (print-tree child))))

;; (defmethod get-leaf ((root qtree-node) path)
;;   (with-slots (childs) root
;;     (cond
;;       ((or (null root)
;;            (null path)) root)
;;       (t
;;        (get-leaf (nth (first path) root)  (last path))))))

(defmethod add-black-pixel ((node qtree-node) x y &key (path nil))
  (if (= 1 (node-size node))	; hit the bottom
      (progn           ; todo remove progn
        (setf (node-color node) +black-color+))
      ;; else
      (let ((half-size (/ (node-size node) 2)) (index 3))

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

       (when (null (nth index (node-childs node)))
         (let ((child (make-instance 'qtree-node
                                     :size half-size
                                     :level (1+ (node-level node))
                                     :parent node
                                     :color +white-color+
                                     :orient index)))
           (setf (node-childs node) (update-list (node-childs node) index child))))

       (add-black-pixel (nth index (node-childs node))  x y
                        :path (cons (node-orient node) path) ))))

(defun get-node-by-path (node path)
  "Recursive go throgh the tree and find node with PATH."
  (when (null node) (error "Node path can't be found."))
  (if (null path)
      node
      (if (null (nth (first path) (node-childs node)))
          node
          (get-node-by-path (nth (first path) (node-childs node)) (rest path)))))


(defun label-neib-r (node &key path state)
  "Label all childs for NODE. PATH is a reversed path to NODE from root of the tree.
STATE is a property list, must have at least :CONDITION, :LABEL and :ROOT-NODE properties."
  (let ((condition (getf state :condition))
        (label-for-node (getf state :label))
        (root-node (getf state :root-node)))
    ;; condition and mark may be separate functions
    (if (or
         (not (= 4 (node-size node)))                ; label only not labeled nodes with size 4
         (< 0 (node-label node)))
        nil
        ;; (return-from label-neib-r nil)                 ;
        (let* ((pathes-list (get-pathes-list path)) stack-for-pathes nd)
          (setf (node-label node) label-for-node)
          (loop do
               (setf stack-for-pathes nil)
               (dolist (p pathes-list)
                 (setf nd (get-node-by-path root-node (reverse  p)))
                 (when (funcall condition nd :path path :state state)
                   ;;                    (format t "SET label (~a) -> (~a) ~%" p (node-label node))
                   (push p stack-for-pathes)))

               (setf pathes-list nil)
               (dolist (p stack-for-pathes)
                 (when (not (member p pathes-list :test #'equal))
                   (push p pathes-list)))
             while (not (null stack-for-pathes)))

          ;; update label
         (incf label-for-node)
         t))))

(defmethod label-neib ((tree qtree) condition)
  (let ((state `(:label 1 :condition ,condition :root-node ,(tree-root-node tree))))
    (map-tree tree #'label-neib-r :path nil :state state )))
;;------------------------------------------------------------
;; functions section
;;------------------------------------------------------------
(defun offset (path)
  "Get node offset from left top corner of image."
    (let ((x 0)
          (y 0)
          (half-size (/ (expt 2 (1- (length path))) 2)))
      (dolist (i (rest (reverse path)))
        (setf x (+ x (* (logand 1 i)  half-size)))
        (setf y (+ y (* (ash (logand 2 i) -1) half-size)))
        (setf half-size (/ half-size 2)))
      (list x y)))

;; macro for creating 4 similar functions - finding neibhours on same
;; node level by path.
(defmacro create-find-neib (func-name member-list index-list)
  `(defun ,func-name (path)
       (let ((index ,(first member-list))
             (work-path (copy-list path))
             fork-path)
         (loop while (and
                      (member index ',member-list)
                      (not (null work-path))) do
              (setf index (pop work-path))
              (push (nth index ',index-list) fork-path))
         (append (reverse fork-path) work-path))))

(create-find-neib  bottom-neib (2 3) (2 3 0 1))
(create-find-neib  top-neib    (0 1) (2 3 0 1))
(create-find-neib  left-neib   (0 2) (1 0 3 2))
(create-find-neib  right-neib  (1 3) (1 0 3 2))

(defun get-pathes-list (path)
  "Get list pathes of near nodes by PATH."
  (let ((init-path (rest path))
        (path-list nil)
        (orient (first path)))

    (when (null init-path)
      (return-from get-pathes-list path-list))

    (cond                               ; add two brother nodes
      ((< 0 orient 3)
       (push (cons 0 init-path) path-list)
       (push (cons 3 init-path) path-list))
      (t
       (push (cons 1 init-path) path-list)
       (push (cons 2 init-path) path-list)))

    (case orient                        ; add two other opposite nodes
      (0
       (push (top-neib path) path-list)
       (push (left-neib path) path-list))
      (1
       (push (top-neib path) path-list)
       (push (right-neib path) path-list))
      (2
       (push (bottom-neib path) path-list)
       (push (left-neib path) path-list))
      (3
       (push (bottom-neib path) path-list)
       (push (right-neib path) path-list)))
    path-list))

(defun make-qt (infile &key (outfile (change-extension infile "png")))
  "Make a quadtree of image (INFILE) and save it to OUTFILE."
  (declare (ignore outfile))
  (let* ((image-path (resize-to-200-dpi infile :dest-filename (get-temp-png-file)))
         (image (load-image image-path))
         (w (png:image-width image))
         (h (png:image-height image))
         (ht (image-to-hashtable image))
         (qt (make-instance 'qtree :img-hash ht :width w :height h)))
    (save-image (hashtable-to-image ht w h) #p"/tmp/out.png") ;todo delete this
    (format t "Tree created successfuly ~%")
    qt))

;; create image and save nodes to it
(defun draw-rect (image rect &key (color 0) )
  "Draw rect on grayscale IMAGE. RECT is (:start (x y) :size (x y)."
  (let* ((x0 (first (getf rect :start)))
        (y0 (second (getf rect :start)))
        (x1 (+ x0 (first (getf rect :size))))
        (y1 (+ y0 (second (getf rect :size)))))
    (loop for x from x0 to x1 do
         (loop for y from y0 to y1 do
              (setf (aref image y x 0) color)))))


(defun fill-image (image color)
  "Fill image with COLOR."
  (draw-rect image `(:start (0 0)
                            :size ,(list  (1- (png:image-width image))  (1- (png:image-height image))))
             :color color))


(defun dump-tree-image (tree image-filename)
  "Dump TREE nodes as image to file. This function for debuging."
  (let ((image (png:make-image (tree-size tree) (tree-size tree) 1 8)))
    (fill-image image 255)
    (map-tree tree #'(lambda (node &key path state)
                       (declare (ignore state))
                       (when (and (= 1 (node-size node))  (eq (node-color node) +black-color+))
                         (let* ((xy (offset path))
                                (x (first xy))
                                (y (second xy)))
                           (setf (aref image y x 0) +black-color+)))))
    (save-image image image-filename)))
