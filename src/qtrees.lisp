;; this module purpose is proceed files with quad trees and find text objects

(in-package #:cl-vectorizer)

;; orients: northwest, northeast, southwest, southeast
(defvar +orients+ '(nw ne sw se))
;; use this color, because color constants in packages.lisp differ
(defconstant +white-color+ 0)
(defconstant +black-color+ 1)

(defun update-list (list index new-value)
  "Update one element of list (with `index`) with `new-value`. Create a copy of list and
then update it."
  (let ((new-list (copy-list list)))
    (setf (nth index new-list) new-value)
    new-list))

;;------------------------------------------------------------
;; class definitions
;;------------------------------------------------------------
(defclass qtree-node nil
  ((size     :initarg :size)
   (level    :initarg :level)
   (label    :initarg :label :initform 0)
   (childs   :initarg :childs :initform '(nil nil nil nil))
   (parent   :initarg :parent :initform nil)
   (color    :initarg :color :initform nil)
   (orient   :initarg :orient :initform nil) ;'root)
   (density  :initarg :density :initform 0))
  (:documentation "Quadtree base node."))

(defclass qtree nil
  ((image-hash    :initarg :image-hash :initform nil)
   (size          :initarg :size :initform nil)
   (root-node     :initarg :root-node :initform nil))
  (:documentation "Quadtree class."))
;;------------------------------------------------------------
;; initialize-instance section
;;------------------------------------------------------------
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
;;------------------------------------------------------------
;; generics section
;;------------------------------------------------------------
(defgeneric print-qtree-node (node stream)
  (:documentation "Print qtree-node slots."))

(defgeneric offset (node root-size)
  (:documentation "Get node offset from right top corner of image." ))

(defgeneric recalc-colors (node)
  (:documentation "Recalculate color value for node.  Color of node is a sum of all
undelying nodes. Color of node with size 1 is +white-color+ or +black-color+."))

(defgeneric map-tree (root eval-func  &key path state)
  (:documentation "Visit all nodes of tree from root and below, evaluate eval-func on each
  of nodes."))

(defgeneric dump-tree (root hash)
  (:documentation "Dump tree to a hash table."))

(defgeneric print-tree (root)
  (:documentation "Print tree nodes."))

(defgeneric get-leaf (root path)
  (:documentation "Get tree node by path. Path is a list of numbers 0, 1, 2 and 3."))

(defgeneric add-black-pixel (root x y)
  (:documentation "Add black pixel to quadtree."))

(defgeneric label-neib (tree cond)
  (:documentation "Recursive label tree nodes answer the condition, implemented in `cond`
  function."))

;;------------------------------------------------------------
;; methods section
;;------------------------------------------------------------
(defmethod print-qtree-node ((node qtree-node) stream)
  (with-slots (size level label color density) node
    (print-unreadable-object (node stream :type t)
      (format stream "size: ~a, label: ~a, level: ~a, color: ~a, density: ~a~%"
              size level label color density))))

(defmethod reset-tree-label((tree qtree))
  (with-slots (current-label) tree
    (setf current-label 1)))

(defmethod update-tree-label ((tree qtree))
  (with-slots (current-label) tree
    (incf current-label)))

(defmethod offset ((node qtree-node) root-size)
  (with-slots (path) node
    (let ((x 0) (y 0))
      (dolist (i path)
        (setf x (+ x (* (logand 1 i) (/ root-size 2))))
        (setf y (+ y (* (ash (logand 2 i) -1) (/ root-size 2))))
        (setf root-size (/ root-size 2)))
      (list x y))))

(defmethod recalc-colors ((tree qtree-node))
  (with-slots (root-node) tree
    (recalc-colors root-node)))

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

(defmethod map-tree ((tree qtree)  eval-func &key (path nil) (state nil))
  (with-slots (root-node) tree
    (with-slots (childs) root-node
      (dolist (child childs)            ; map root childs, this place
        (when (not (null child))        ; is good for parallel computing
          (map-tree child eval-func :path path :state state))))))

(defmethod map-tree ((root qtree-node)  eval-func &key path state)
  (funcall eval-func root :path path :state state)
  (with-slots (childs size orient) root
    (dolist (child childs)
      (when (not (null child))
        (map-tree child eval-func :path (cons orient path) :state state)))))
    ;; (map 'list #'(lambda (child)
    ;;                (cond
    ;;                  ((null child) nil)
    ;;                  (t
    ;;                   (map-tree child eval-func :path (cons orient path) :state state)))) childs)))

;; (defmethod dump-tree

(defmethod print-tree ((tree qtree))
  (with-slots (root-node) tree
    (print-tree root-node)))

(defmethod print-tree ((root qtree-node))
  (with-slots (childs size) root
    (format t "node~a, childs = ~a~%" root childs)
    (dolist (child childs)
      (when (not (null child))
        (print-tree child)))))

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
         (add-black-pixel new-root  x y))))))

(defun get-node-by-path (node path)
  "Recursive go throgh the tree and find node with PATH."
  (when (null node) (error "Node path can't be found."))
  (if (null path)
      node
      (with-slots (childs) node
        (if (null (nth (first path) childs))
            node
            (get-node-by-path (nth (first path) childs) (rest path))))))


(defun label-neib-r (node &key path state)
  "Label all childs for NODE. PATH is a reversed path to NODE from root of the tree.
STATE is a property list, must have at least :condition, :label and :root-node properties."
  (let ((condition (getf state :condition))
        (label-for-node (getf state :label))
        (root-node (getf state :root-node)))
  (with-slots (size label) node
    (cond
      ((or
        (not (= 4 size))                ; label only not labeled nodes with size 4
        (< 0 label)) (return-from label-neib-r nil))                 ;

      (t
       (let* ((pathes-list (get-pathes-list path)) 
              stk nd)
         (setf label label-for-node)
         (loop do
              (setf stk nil)

              (dolist (p pathes-list)
                (setf nd (get-node-by-path root-node (reverse  p)))
                (when (and
                       (funcall condition nd)
                       (eq 0 (slot-value nd 'label)))
                (with-slots (label) nd
                  (setf label label-for-node)
                  (push p stk))))

              (setf pathes-list nil)
              (dolist (p stk)
                (when (not (member p pathes-list :test #'equal))
                  (push p pathes-list)))
              while (not (null stk)))


         ;; update label
         (incf label-for-node)

         t))))))

(defmethod label-neib ((tree qtree) condition)
  (let ((state `(:label 1 :condition ,condition :root-node ,(slot-value tree 'root-node))))
    (map-tree tree #'label-neib-r :path nil :state state )))

;; macro for creating 4 similar functions - finding neibhours on same
;; node level by path.
(defmacro create-find-neib (name member-list index-list)
  `(defun ,name (path)
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
  "Get list pathes of near nodes."
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
