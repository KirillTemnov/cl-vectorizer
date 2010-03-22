
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
    (when (and (> x-dpi +min-dpi+ ) (> y-dpi +min-dpi+))
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
    

;; (setf (getf *settings* :working-dir-in) #p"/storage/lisp/sbcl/vector-test/in/")
;; (setf (getf *settings* :working-dir-out) #p"/storage/lisp/sbcl/vector-test/out/")

;;(resize-to-200-dpi #p"01.tif" :dest-filename #p"11.png")

;; (convert-image #p"02.tif" :dest-filename #p"02.png" :options '("-sharpen" "3x3" "-threshold" "60%" "-colors" "2"))
;; (get-out-path  #p"02.tif")

;; (get-image-info (namestring (get-in-path #p"01.tif")))


;; (defun thin-file (filename   &key (outfile-name (getf *settings* :temp-pgm-file)))
;;   "Thin a file.
;; Resize and threshold file, then, convert to PGM, then thin and save to
;; outfile-name. Result also saves to png.
;;    "
;;   (resize-to-200-dpi filename)
;;   (let ((pgm-image (get-out-path (change-extension filename "pgm")))
;; 	(out-pgm-image (get-out-path outfile-name)))
;;     (thin-image-file (namestring pgm-image) (namestring out-pgm-image))
;;     ;;(dumb-convert out-pgm-image (get-out-path (change-extension filename "png")))
;; ))

;; (thin-image-file (namestring (get-out-path #p"Data003.pgm")) (namestring (get-out-path (getf *settings* :temp-pgm-file))))

;;(time (thin-file #p"Data003.tif"))

;; WORKING SAMPLE
;; (convert-image  "/storage/lisp/sbcl/vector-test/in/01.tif"
;; 		"/storage/lisp/sbcl/vector-test/in/01.png"
;; 		:options '("-adaptive-resize" "25%" "-colors" "2" "-threshold" "55%"))

;; (defun thin-image-file (filename outfile)
;;   (let* ((info (get-image-info filename))
;; 	 (dimensions (list (getf info :width) (getf info :height)))
;; 	 (data (cdr (read-pgm-file filename))))
;;     (write-hash-as-pgm-file-with-dimensions (thin-image data) dimensions outfile)
;;     't))

(defun thin-image-file (infile &key (outfile (change-extension infile "png")))
  "Thin image in one file and save to another"
  (let* ((image-path (resize-to-200-dpi infile :dest-filename (get-temp-png-file)))
	 (image (load-image image-path))
	 (w (png:image-width image))
	 (h (png:image-height image))
	 (ht (thin-image-hash (image-to-hashtable image))))
    (save-image (hashtable-to-image ht w h) outfile)
    t))


;; +TEST+
;;(get-image-info "./out/03.tif")
;;(get-image-info "./in/01.tif")
;;(guess-format-by-info (get-image-info  "./in/Data003.tif"))
;;(guess-format-by-info   '(:YRESOLUTION 300 :XRESOLUTION 300 :HEIGHT 3794 :WIDTH 2759))
;;(guess-format   "./in/03.tif" )
;; -TEST-
 
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



    



;;------------------------------------------------------------------------------
;;------------------------------------------------------------------------------

;; (defun main ()
;;   (format t "first ~s ~%" (tokenize (first (run-command-return-output (get-identify-path) :arguments '("./out/03.tif"))))))
;;  (pprint (run-command-return-output "/bin/ls" :arguments '("-l"))))


;; +TEST+  Test of read-write file
;; (time (let* ((V (read-pgm-file "/storage/lisp/sbcl/vector/out/03.pgm"))
;;        (data (rest V)))
;;   (format t "File was readed succesful ~d ~%" (length data ))
;;   (write-pgm-file "/storage/lisp/sbcl/vector/out/30.pgm" '(910 1252) data)
;;   (pprint 'ok)))
;; -TEST-
