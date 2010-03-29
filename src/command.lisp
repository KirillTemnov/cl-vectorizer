
(in-package #:cl-vectorizer)

(defun run-command (command &key (arguments nil) (input nil) (output t))
  "run shell command. system path to command must be full."
  (sb-ext:run-program command arguments :input input :output output))

(defun run-command-return-output (command &key (arguments nil) (input nil))
  "run shell command and return it's ousput as a tokens string"
  (let ((results-list nil))
    (with-open-stream (out 
		       (sb-ext:process-output 
			(sb-ext:run-program command arguments :input input :output :stream)))
      (loop :for line = (read-line out nil nil)
	 :while line
	 :do (setf results-list (append results-list (list line)))))
    results-list))

(defun get-image-info (path)
  "get information about image. 
return plist (:width int-value :height int-value 
              :xresolution int-value :yresolution int-value)
Resolution is in DPI"
  (let* ((image-plist nil) 
	 ;; prevent output with (first (last ...))
	(str-data (first (last (run-command-return-output (get-identify-path) :arguments (list "-format" "'%[width] %[height] %[xresolution] %[yresolution]'" path)))))
	(str-data-formated (str-list-to-int-list (subseq str-data 1 (- (length str-data) 1)))))
    (setf (getf image-plist :width) (first str-data-formated))
    (setf (getf image-plist :height) (second str-data-formated))
    (setf (getf image-plist :xresolution) (third str-data-formated))
    (setf (getf image-plist :yresolution) (fourth str-data-formated))
    image-plist))

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
  "Thin image in one file and save to another"
  (let* ((image-path (resize-to-200-dpi infile :dest-filename (get-temp-png-file)))
	 (image (load-image image-path))
	 (w (png:image-width image))
	 (h (png:image-height image))
	 (ht (thin-image-hash (image-to-hashtable image)))
	 lines-ht)
    (format t "vectorize ... ~%" )
    (setf lines-ht (vectorize-hash ht))
    ;; (format t "megre lines ... ~%")
    ;; (remove-hash-lines-duplecates lines-ht)
    ;; (setf lines-ht (remove-hash-lines-duplecates (merge-near-lines lines-ht)))
    (format t "export to svg ... ~%")
    ;;
    (save-hashtable-as-svg  lines-ht (format nil "~apx" w) (format nil "~apx"  h))
    (save-image (hashtable-to-image ht w h) (get-out-path outfile))
    lines-ht))

(defun guess-format (image-path)
  "return format of image based on it's DPI.
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

