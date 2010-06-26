
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
  ""
  (run-command (get-convert-path) :arguments (list (namestring from-path) (namestring to-path))))

(defun resize-to-200-dpi (image-name &key (dest-filename) )
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
	    (> x-dpi +min-dpi+ ) 
	    (> y-dpi +min-dpi+)))
	(setf w (round (/ w (/ x-dpi +min-dpi+))))
	(setf h (round (/ h (/ x-dpi +min-dpi+))))
	  )
    (convert-image image-name
		   :dest-filename save-filename
		   :options (list "-sharpen" (getf *settings* :sharpen)
				  "-threshold"  (get-threshold-bin)
				  "-colors" (getf *settings* :colors)
				  "-adaptive-resize" (format nil "~sx~s" w h)))
    save-filename))
    

(defun thin-image-file (infile &key (outfile (change-extension infile "png")))
  "Thin image in one file and save to another."
  (let* ((image-path (resize-to-200-dpi infile :dest-filename (get-temp-png-file)))
	 (image (load-image image-path))
	 (w (png:image-width image))
	 (h (png:image-height image))
	 (ht (thin-image-hash (image-to-hashtable image)))
	 (manager (create-svg-manager  (format nil "~apx" w) (format nil "~apx"  h)))
	 lines-ht)
	 
    (when (get-debug-mode) (format t "format image ~a ... ~%"  (get-out-path outfile)))

    (save-image (hashtable-to-image ht w h) (get-out-path outfile))

    (when (get-debug-mode) 
      (format t "vectorize ... ~%" )
      (format t "Total lines ~a ~%" (hash-table-count ht ))
      )
    
    (setf lines-ht (merge-near-lines (vectorize-hash ht)))
    
    (when (get-debug-mode) (format t "export to svg ... ~%"))
    (remove-hash-lines-duplicates lines-ht)
    

    (setf manager (hashtable-lines-to-svg-manager lines-ht manager))
    
    (flush-manager manager "out.svg")
    
    lines-ht))

(defun get-image-circles (infile &key (outfile (change-extension infile "png")))
  "Thin image and extract circles from it."
  (let* ((image-path (resize-to-200-dpi infile :dest-filename (get-temp-png-file)))
	 (image (load-image image-path))
	 (w (png:image-width image))
	 (h (png:image-height image))
	 (ht (thin-image-hash (image-to-hashtable image)))
	 (manager (create-svg-manager  (format nil "~apx" w) (format nil "~apx"  h)))
	 circles-hash
	 lines-ht)
    (when (get-debug-mode) (format t "Creating circles, image have ~a points" (hash-table-count ht)))
    (save-image (hashtable-to-image ht w h) (get-out-path outfile))


    (setf circles-hash (find-circles ht (get-max-circle-diameter)))


    (loop for circle being the hash-key of circles-hash do
	 (list-points-to-svg-manager (gethash circle circles-hash) manager))

    (hashtable-circles-to-svg-manager circles-hash manager) 

    (add-entity manager (make-svg-image outfile))
    (flush-manager manager "out.svg")))

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

