;;; -*- Mode: Lisp -*-

(defpackage #:cl-vectorizer-asd
  (:use #:cl #:asdf))
 
(in-package #:cl-vectorizer-asd)
 
(defsystem #:cl-vectorizer
  :name "cl-vectorizer"
  :author "Kirill Temnov <allselead@gmail.com>"
  :maintainer "Kirill Temnov <allselead@gmail.com>"
  :description "Vectorizer for raster images"
  :long-description "Vectorizer for raster images. Can save result to dxf,"
  :components ((:module "src"
			:components ((:file "packages")
				     (:file "helpers" :depends-on ("packages"))
				     (:file "command" :depends-on ("packages" "helpers"))
				     (:file "zong-suen" :depends-on ("helpers" "packages"))
;;				     (:file "cmd-line" :depends-on ("helpers"))
	       ;; (:file "file" :depends-on ("module"))
               )))
  :depends-on (:sb-dxf :png))
