;;; -*- Mode: Lisp -*-

(defpackage #:cl-vectorizer-asd
  (:use #:cl #:asdf))
 
(in-package #:cl-vectorizer-asd)
 
(defsystem #:cl-vectorizer
  :name "cl-vectorizer"
  :author "Kirill Temnov <allselead@gmail.com>"
  :maintainer "Kirill Temnov <allselead@gmail.com>"
  :description "Vectorizer for raster images"
  :long-description "Vectorizer for raster images. Can save result to dxf or svg."
  :components ((:module "src"
			:components ((:file "packages")
				     (:file "helpers" :depends-on ("packages"))
				     (:file "command" :depends-on ("packages" "helpers"))
				     (:file "zong-suen" :depends-on ("packages" "helpers"))
				     (:file "vectorize" :depends-on ("packages" "helpers"))
				     (:file "hough" :depends-on ("packages" "helpers"))
				     (:file "svg" :depends-on ("packages" "helpers"))
	       ;; (:file "file" :depends-on ("module"))
               )))
  :depends-on (:sb-dxf :png))
