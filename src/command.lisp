
(in-package #:cl-vectorizer)

(defun run-command (command &key (arguments nil) (input nil) (output t))
  "Run shell command. system path to command must be full."
  (sb-ext:run-program command arguments :input input :output output))

(defun run-command-return-output (command &key (arguments nil) (input nil))
  "Run shell command and return it's ousput as a tokens string."
  (let ((results-list nil))
    (with-open-stream (out
		       (sb-ext:process-output
			(sb-ext:run-program command arguments :input input :output :stream)))
      (loop :for line = (read-line out nil nil)
	 :while line
	 :do (setf results-list (append results-list (list line)))))
    results-list))

(defun get-image-info (path)
  "Get information about image.
 `path` is a full path to image.

Return plist:
  (:width int-value :height int-value :xresolution int-value :yresolution int-value)
Resolution is in DPI"
  (let* ((image-plist nil)
	(str-data (first (last (run-command-return-output (get-identify-path) :arguments (list "-format" "'%[width] %[height] %[xresolution] %[yresolution]'" path)))))
	(str-data-formated (str-list-to-int-list (subseq str-data 1 (- (length str-data) 1)))))
    (setf (getf image-plist :width) (first str-data-formated))
    (setf (getf image-plist :height) (second str-data-formated))
    (setf (getf image-plist :xresolution) (third str-data-formated))
    (setf (getf image-plist :yresolution) (fourth str-data-formated))
    image-plist))

(defun cut-image-sizes (width height crop-width crop-heigth)
  "Get image sizes for crop operation. Returns a list, containing sublists like
 (crop-num (offset-x offset-y)), where crop-num - number of croping part, offset-x and offset-y
 - offsets from left up corner of source image.

Examples:
>(cut-image-sizes 800 600 200 100)

 ((0 (0 0)) (1 (200 0)) (2 (400 0)) (3 (600 0)) (4 (200 100)) (5 (400 100))
 (6 (600 100)) (7 (200 200)) (8 (400 200)) (9 (600 200)) (10 (200 300))
 (11 (400 300)) (12 (600 300)) (13 (200 400)) (14 (400 400)) (15 (600 400))
 (16 (200 500)) (17 (400 500)) (18 (600 500)))

> (cut-image-sizes 800 600 1000 1000)

 ((0 (0 0)))
"
  (let ((crop-list '((0 (0 0)))))
    (let ((times-x (floor (/ width crop-width)))
	  (times-y (floor (/ height crop-heigth)))
	  (i 0) l (y 0))
      (when (= (rem width crop-width) 0) (decf times-x))
      (when (= (rem height crop-heigth) 0) (decf times-y))
      (loop
	 (when (> y times-y) (return crop-list))
	 (setf l (loop for x from 1 to times-x collecting
		      (list (+ x i) (list (* crop-width x) (* y crop-heigth)))))
	 (incf i times-x)
	 (setf crop-list (append crop-list l))
	 (incf y)))))


(defun split-image (source-filename)
  "Split image to samller parts fized size (+sheet-width+ and +sheet-height+).
Converted files moved to output folder.
"
  (let* ((info (get-image-info (namestring (get-in-path source-filename))))
	 (image-sizes (cut-image-sizes  (getf info :width) (getf info :height) +sheet-width+ +sheet-height+)))
    (dolist (image-size image-sizes)
      (convert-image source-filename
		     :dest-filename
		     (change-extension
		      (add-to-filename source-filename
				       (format nil "-~a" (first image-size))) "png")
		     :options   (list "-crop"
				      (format nil "~ax~a+~a+~a" +sheet-width+ +sheet-height+ (first (second image-size)) (second (second image-size))))))))


(defun convert-image (source-filename  &key (dest-filename source-filename) options)
  "Convert one image to anther via imagemagick.
Options:
  -sharpen AxB  -- charpen by A x B mask
  -threshold CC% -- set threshold by all channels in persent
  -colors [2,8,16,256]  -- change color palete
  -resize DD%
  -adaptive-resize XxY           -- resize image
TODO Add default values.
"
  (run-command (get-convert-path)
	       :arguments
	       (concatenate 'list (list (namestring (get-in-path source-filename)))
			    options
			    (list (namestring (get-out-path dest-filename))))))

(defun dumb-convert (from-path to-path)
  "Convert image from one format to another."
  (run-command (get-convert-path) :arguments (list (namestring from-path) (namestring to-path))))

(defun resize-to-fixed-dpi (image-name &key (dest-filename)  (final-dpi +min-dpi+))
  (let* ((info (get-image-info (namestring (get-in-path image-name))))
	 (x-dpi (getf info :xresolution))
	 (y-dpi (getf info :yresolution))
	 (w (getf info :width))
	 (h (getf info :height))
	 (save-filename (or dest-filename (change-extension image-name "png"))))
    (when (and 				; if no info about dpi is provided
	   (not (eq nil x-dpi))		; image will not be resized
	   (not	(eq nil y-dpi))
	   (and
	    (> x-dpi final-dpi )
	    (> y-dpi final-dpi)))
	(setf w (round (/ w (/ x-dpi final-dpi))))
	(setf h (round (/ h (/ x-dpi final-dpi))))
	  )
    (convert-image image-name
		   :dest-filename save-filename
		   :options (list "-sharpen" (getf *settings* :sharpen)
				  "-threshold"  (get-threshold-bin)
				  "-colors" (getf *settings* :colors)
				  "-adaptive-resize" (format nil "~sx~s" w h)))
    save-filename))


(defun thin-image-file (infile &key (max-distance 100)
                        (outfile (change-extension infile "png")))
  "Thin image in one file and save to another."
  (let* ((image-path (resize-to-fixed-dpi infile :dest-filename (get-temp-png-file) :final-dpi 150))
	 (image (load-image image-path))
         (w (png:image-width image))
         (h (png:image-height image))
	 (ht (thin-image-hash (image-to-hashtable image)))
	 (manager (create-svg-manager  (format nil "~apx" w) (format nil "~apx"  h)))
         (dxf-manager (sb-dxf:create-manager :filename (change-extension infile "dxf")))
         hash-4-circles
	 lines-ht
         circles-hash)

    (when (get-debug-mode) (format t "format image ~a ... ~%"  (get-out-path outfile)))

    (save-image (hashtable-to-image ht) (get-out-path outfile))

    (when (get-debug-mode)
      (format t "vectorize ... ~%" )
      (format t "Total lines ~a ~%" (hash-table-count ht ))
      )

    (setf lines-ht (merge-near-lines (vectorize-hash ht)))

    (when (get-debug-mode) (format t "export to svg ... ~%"))
    (remove-hash-lines-duplicates lines-ht)

    (format t "Circles: ")
    (setf hash-4-circles (filter-hash lines-ht #'(lambda (line) (< (third line) 30))))
    (format t "Total lines for circles: ~A~%" (hash-table-count hash-4-circles))
    (setf circles-hash (find-circles2 lines-ht max-distance 10))
    (print-hash circles-hash)
    (hashtable-circles-to-svg-manager circles-hash manager)

    (setf manager (hashtable-lines-to-svg-manager lines-ht manager
                                                  :short-lines-color "magenta"))

    (flush-manager manager #p"out.svg")



    (hashtable-lines-to-dxf-manager lines-ht dxf-manager)

    (format t "Points for circles: ~A~%" (hash-table-count  hash-4-circles))
    (sb-dxf:flush-manager dxf-manager)
;;    (format t "Poccibly circles: ~A~%" (find-circles2 hash-4-circles 600 20))

    lines-ht))

(defun get-image-circles (infile min-radius &key (max-radius-error 3)
                          (outfile (change-extension infile "png"))
                          (window-size 200))
  "Thin image and extract circles from it."
  (let* (;;(tree (make-qt infile))
;;	 (ht (vectorize-hash->points (tree-slice->hash tree 8)))
         (ht (image-to-hashtable (load-image infile)))
;;	 (ht  (tree-slice->hash tree 2)) ; 8 -> 1
;;         (points-list (hashtable-keys-to-list ht))
         (possibly-circles (find-possibly-circles ht
                                                  :min-radius min-radius
                                                  :max-radius-error max-radius-error
                                                  :window-size window-size))
         (max-coords (get-max-coordinates ht))
	 (manager (create-svg-manager
                   (format nil "~apx"
                           (first max-coords))
                           (format nil "~apx"  (second max-coords))))
	 circles-hash)
    (format t "BEFORE SAVE~%"  )
    (setf ht (thin-image-hash ht))
    (save-image (hashtable-to-image ht) (get-out-path outfile))

    (format t "<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<----------AFTER SAVE,,,,~%"  )


    ;; (save-image (hashtable-to-image points-ht) (get-out-path outfile))

    (setf circles-hash (find-circles ht possibly-circles :max-radius-error max-radius-error))


    ;; show points on image
    ;; (loop for circle being the hash-key of circles-hash do
    ;; 	 (list-points-to-svg-manager (gethash circle circles-hash) manager))

    (hashtable-circles-to-svg-manager circles-hash manager)
    (draw-grid manager window-size (first max-coords) (second max-coords) :color "blue")
    (draw-grid manager window-size (first max-coords) (second max-coords)
               :color "green"
               :offset-x (floor (/ window-size 2))
               :offset-y (floor (/ window-size 2)))

    (add-entity manager (make-svg-image outfile))
    (flush-manager manager #p"out.svg")))

(defun get-image-circles2 (infile min-radius &key (outfile (change-extension infile "png")))
  "Thin image and extract circles from it."
  (let* ((tree (make-qt infile))
;;	 (ht (vectorize-hash->points (tree-slice->hash tree 8)))
	 (ht  (tree-slice->hash tree 8))
         (max-coords (get-max-coordinates ht))
	 (manager (create-svg-manager
                   (format nil "~Apx"
                           (first max-coords))
                           (format nil "~Apx"  (second max-coords))))
	 circles-hash
	 lines-ht
	 points-ht)
    (declare (ignore lines-ht points-ht))
    ;; (format t "SAVING IMAGE------------------------------>>>>>>>>>>~%"  )
    ;; (print-hash ht)
    (save-image (hashtable-to-image ht) (get-out-path outfile))

    (format t "<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<----------AFTER SAVE,,,,~%"  )
    ;; (setf lines-ht (merge-near-lines (vectorize-hash ht)))

    ;; (setf points-ht (hashlines-to-hashpoints lines-ht))
    ;; ;; test

    ;; (save-image (hashtable-to-image points-ht) (get-out-path outfile))

    ;; (when (get-debug-mode)
    ;;   (format t "Creating circles, image have ~a points" (hash-table-count points-ht)))


    ;; (setf circles-hash (find-circles points-ht (get-max-circle-diameter)))
    (setf circles-hash  (find-circles2 ht (get-max-circle-diameter) min-radius))


    ;; show points on image
    ;; (loop for circle being the hash-key of circles-hash do
    ;; 	 (list-points-to-svg-manager (gethash circle circles-hash) manager))

    (hashtable-circles-to-svg-manager circles-hash manager)

    (add-entity manager (make-svg-image outfile))
    (flush-manager manager #p"out.svg")))


(defun guess-format (image-path)
  "Return format of image based on it's DPI.
list of formats: 'A0 'A1 'A2 'A3 'A4 'A5 'A6 'A7"
  (guess-format-by-info (get-image-info image-path)))

(defun guess-format-by-info (info)
  "return format of image, described with info plist
looks like: (:width int-value :height int-value
              :xresolution int-value :yresolution int-value).
list of formats: 'A0 'A1 'A2 'A3 'A4 'A5 'A6 'A7"
  (let* ((formats-list (list '(841 1189 'a0) '(594 841 'a1) '(420 594 'a2) '(297 420 'a3) '(210 297 'a4)
			     '(148 210 'a5) '(105 148 'a6) '(74 105 'a7)))
	 (x-ratio (/ (getf info :xresolution) +inch+))
	 (y-ratio (/ (getf info :yresolution) +inch+))
	 (w (/ (getf info :width) x-ratio))
	 (h (/ (getf info :height) y-ratio))
	 (min '(10 nil)))
    (map 'list #'(lambda (x)                     ;; find minimum value (min)
		   (if (< (first x) (first min))
		       (setf min x)))
	 (map 'list #'(lambda (x) (list ;; list of delta values of formats (float 'a0) (float 'a1) ...
				   (min
				    (+
				     (abs (- 1 (/ (first x) w)))
				     (abs (- 1 (/ (second x) h))))
				    (+
				     (abs (- 1 (/ (first x) h)))
				     (abs (- 1 (/ (second x) w)))))
				   (third x))) formats-list))
    (second min)))

