
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
	   #:resize-to-fixed-dpi
	   #:thin-image-file
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
(defvar version "0.4.0" "Package version.")
;; format sizes
(defvar +a0+ '(841 1189))
(defvar +a0-landscape+ '(1189 841))
(defvar +a1+ '(594 841))
(defvar +a1-landscape+ '(841 594))
(defvar +a2+ '(420 594))
(defvar +a2-landscape+ '(594 420))
(defvar +a3+ '(297 420))
(defvar +a3-landscape+ '(420 297))
(defvar +a4+ '(210 297))
(defvar +a4-landscape+ '(297 210))
(defvar +a5+ '(148 210))
(defvar +a5-landscape+ '(210 148))
(defvar +inch+ 25.4)
(defvar +error-in-quess+ 0.15)
(defvar +min-dpi+ 200.0)
(defvar +min-line-len+ 3 "Minimum length of line, that can be merged with other line without analysing slope angle")
(defvar +max-line-len+ 7 "Max line length, after which slope angle must strict match")
;-------------------------------------------------------------------------------
;; width and height of cutting sheets
(defvar +sheet-width+ 500)
(defvar +sheet-height+ 500)

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

;; debug flag
(setf (getf *settings* :debug-mode) nil)

;; colors 2
(setf (getf *settings* :colors) "2")

;; sharpen
(setf (getf *settings* :sharpen) "3x3")


;; threshold for vectorization
(setf (getf *settings* :threshold) 20)
;; threshold for binarization
(setf (getf *settings* :threshold-bin) "65%")

(setf (getf *settings* :max-angle-on-line) 15)

(setf (getf *settings* :min-angle-on-line) 5)

(setf (getf *settings* :max-slope-angle) 5)

(setf (getf *settings* :circles-tolerance-func)  #'(lambda (circle value)
                                (let ((radius (first circle)))
                                  (cond
                                    ((< 30 radius) (< 7000 value))
                                    ((< 20 radius) (< 6000 value))
                                    ((< 15 radius) (< 5000 value))
                                    ((< 10 radius) (< 4000 value))
                                    ((< 1000 value) t)
                                    (t nil)))))

;; list of radius and max angle between points for this circles
;; ((max-radius1 max-angle1) (max-radius2 max-angle2) ... (max-radiusN max-angleN))
(setf (getf *settings* :angles-step-for-circle)
      '((10 30)
	(20 28)
	(30 25)
	(40 20)
	(50 18)
	(100 15)
	(200 12)
	(300 10)
	(500 8)
	(1000 5)
	(10000 3)))
;;------------------------------------------------------------------------------
;; accessors
;;-----------------------------------------------------------------------------
(defun get-identify-path nil
  (getf *settings* :identify))

(defun get-convert-path nil
  (getf *settings* :convert))

(defun get-temp-png-file nil
  "Get path of temporary png file."
  (merge-pathnames (getf *settings* :working-dir-out) (getf *settings* :temp-png-filename)))


(defun concat-atoms (&rest args)
  "Concatenate atoms as strings.
Example:
 (concat-atoms 'a 'b 'c)

abc
"
  (intern (apply #'(lambda (&rest args)
		     (with-output-to-string (s)
		       (dolist (a args) (princ a s))))
		 args)))

;; macro for creating set- and get- properties
(defmacro property (name &key (set-docstring "") (get-docstring ""))
  `(progn
     (defun ,(concat-atoms 'set- name) (value)
       ,set-docstring
       (setf (getf *settings* ,(intern (symbol-name name) :keyword)) value))
     (defun ,(concat-atoms 'get- name) nil
       ,get-docstring
       (getf *settings* ,(intern (symbol-name name) :keyword)))))

(property threshold
	  :set-docstring "Set threshold for vectorization (int value)."
	  :get-docstring "Get threshold for vectorization.")

(property threshold-bin
	  :set-docstring "Set threshold for image binarization (string with persent sign)."
	  :get-docstring "Get threshold for image binarization.")

(property working-dir-in
	  :set-docstring "Set working dir for input images."
	  :get-docstring "Get working dir for input images.")

(property working-dir-out
	  :set-docstring "Set working dir for output images and data."
	  :get-docstring "Get working dir for output images and data.")

(property debug-mode
	  :set-docstring "Set debug-mode. If debug-mode set to t, debug messages prints to stdout."
	  :get-docstring "Get debug-mode state (t or nil - default).")

(property max-angle-on-line
	  :set-docstring "Set maximum angle deviation from one point to other in one straight line."
	  :get-docstring "Get maximum angle deviation from one point to other in one straight line.")

(property max-slope-angle
	  :set-docstring "Set maximum slope angle. If slope angle between 2 lines less than slope angle, they are parallel."
	  :get-docstring "Get maximum slope angle.")

(property line-search-radius
	  :set-docstring "Set radius in points for searching near lines."
	  :get-docstring "Get radius for searching near lines.")

(property max-noise-line-length
	  :set-docstring "Set maximal length of noise lines."
	  :get-docstring "Get maximal length of noise lines.")

(property max-small-line-length
          :set-docstring "Set maximum length for short lines."
          :get-docstring "Get maximum length for short lines.")

(property max-length-to-restore
          :set-docstring "Set maximum length that can be added to line (from one side), during it restore."
          :get-docstring "Get maximum length that can be added to line (from one side), during it restore.")

(property max-circle-diameter
	  :set-docstring "Set maximum diameter for circles, that will be searched by Hough algorithm."
	  :get-docstring "Get maximum diameter for circles, that will be searched by Hough algorithm.")

(property angles-step-for-circle
	  :set-docstring "Set angles for analyzing circles and arcs."
	  :get-docstring "Get angles for analyzing circles and arcs.")

(property circles-tolerance-func             ; todo use for debug purposes
          :set-docstring "Set circles tolerance function change with caution."
          :get-docstring "Get circles tolerance function.")

(property min-histogram-value-of-circle-center
          :set-docstring "Set minimum histogram value of circle center (in persent)."
          :get-docstring "Get minimum histogram value of center center.")

(property min-histogram-value-of-circle-edge
          :set-docstring "Set minimum histogram value of circle edge (in persent)."
          :get-docstring "Get minimum histogram value of center edge.")

(property max-distance-between-lines
          :set-docstring "Set max distance between two lines for merging lines algorithm."
          :get-docstring "Get max distance between two lines for merging lines algorithm.")



(set-line-search-radius 4)

(set-max-noise-line-length 3)

(set-max-length-to-restore 4)

(set-max-small-line-length 10)

(set-max-circle-diameter 10)

(set-min-histogram-value-of-circle-center 30)

(set-min-histogram-value-of-circle-edge 7)

(set-max-distance-between-lines 3)