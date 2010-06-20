
(defpackage #:cl-vectorizer
  (:use #:cl)
  (:export #:+a0+
	   #:+a1+
	   #:+a2+
	   #:+a3+
	   #:+a4+
	   #:+a5+
	   #:get-image-info
	   #:guess-format
	   #:run-command
	   #:run-command-return-output
	   #:convert-image
	   #:dumb-convert
	   #:resize-to-200-dpi
	   #:thin-image-file
;;  hide setting 
	   #:*settings* 
	   #:version
	   #:set-working-dir-in
	   #:set-working-dir-out
	   #:+sheet-width+
	   #:+sheet-height+
   ))

(in-package #:cl-vectorizer)

;;------------------------------------------------------------------------------
;; Formating
;;------------------------------------------------------------------------------
(defconstant version "0.1.4.3" "Package version.")
;; format sizes
(defconstant +a0+ '(841 1189))
(defconstant +a0-landscape+ '(1189 841))
(defconstant +a1+ '(594 841))
(defconstant +a1-landscape+ '(841 594))
(defconstant +a2+ '(420 594))
(defconstant +a2-landscape+ '(594 420))
(defconstant +a3+ '(297 420))
(defconstant +a3-landscape+ '(420 297))
(defconstant +a4+ '(210 297))
(defconstant +a4-landscape+ '(297 210))
(defconstant +a5+ '(148 210))
(defconstant +a5-landscape+ '(210 148))
(defconstant +inch+ 25.4)
(defconstant +error-in-quess+ 0.15)
(defconstant +min-dpi+ 200.0)
(defconstant +max-angle-on-line+ 15)
(defconstant +min-angle-on-line+ 5)
(defconstant +max-slope-angle+ 5)
;-------------------------------------------------------------------------------
;; width and height of cutting sheets
(defconstant +sheet-width+ 500)
(defconstant +sheet-height+ 500)

;;------------------------------------------------------------------------------
;; some useful constants here
(defconstant +null+ (code-char 0))
(defconstant +nl+ (code-char 10))
(defconstant +black+ 0) ;;(code-char 0))
(defconstant +white+ 255);;(code-char 255))

;;------------------------------------------------------------------------------

(defvar *settings* ())
;; add default settings here

;; working dir for input images
(setf (getf *settings* :working-dir-in) 
      (merge-pathnames 
       #p"in/"
      #+sbcl *DEFAULT-PATHNAME-DEFAULTS* 
      #+allegro CURRENT-DIRECTORY 
      #+cmucl DEFAULT-DIRECTORY))

;; working dir for output images
(setf (getf *settings* :working-dir-out)       
      (merge-pathnames
       #p"out/"
      #+sbcl  *DEFAULT-PATHNAME-DEFAULTS*  
      #+allegro CURRENT-DIRECTORY 
      #+cmucl DEFAULT-DIRECTORY))
  
;;------------------------------------------------------------------------------
;; convertion options
;;--------------------------------------
;; sharpen
(setf (getf *settings* :sharpen) "3x3")
;; threshold
(setf (getf *settings* :threshold) "60%")
;; colors 2
(setf (getf *settings* :colors) "2")



;; temp filename
(setf (getf *settings* :temp-png-filename) #p"temp.png")
;;(setf (getf *settings* :temp-png-file) #p"temp.png")

;; works for Mac + Linux
(setf (getf *settings* :identify) 
      (cond
	((probe-file #p"/usr/local/bin/identify")  #p"/usr/local/bin/identify")
	((probe-file #p"/opt/local/bin/identify")  #p"/opt/local/bin/identify")
	((probe-file #p"/usr/bin/identify") #p"/usr/bin/identify")
	(t nil)))
		     
(setf (getf *settings* :convert)
      (cond
	((probe-file #p"/usr/local/bin/convert")  #p"/usr/local/bin/convert")
	((probe-file #p"/usr/bin/convert")  #p"/usr/bin/convert")
	(t nil)))

;; threshold for vectorization
(setf (getf *settings* :threshold) 20)
;; threshold for binarization
(setf (getf *settings* :threshold-bin) "65%")

;;------------------------------------------------------------------------------
;; accessors 
;;-----------------------------------------------------------------------------
(defun get-identify-path nil
  (getf *settings* :identify))
;;(defun +identify-path+ nil 
(defun get-convert-path nil
  (getf *settings* :convert))
;;(defun +convert-path+ nil 
(defun get-threshold nil
  (getf *settings* :threshold))

(defun get-threshold-bin nil
  (getf *settings* :threshold-bin))

;(defun +threshold+ nil 
(defun get-temp-png-file nil
  (merge-pathnames (getf *settings* :working-dir-out) (getf *settings* :temp-png-filename)))

(defun set-working-dir-in (path)
  (setf (getf *settings* :working-dir-in) path))

(defun set-working-dir-out (path)
  (setf (getf *settings* :working-dir-out) path))

(defun get-working-dir-in  () (getf *settings* :working-dir-in))

(defun get-working-dir-out () (getf *settings* :working-dir-out))
