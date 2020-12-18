(in-package :cl-user)

;;; change these to real location
(defparameter *wordnet-data-dir* "/home/bob/PQ/Data/Nuvia-Data/WordNet3.0/")
(defparameter *wordnet-source-dir* "/home/bob/bob-wordnet/")

(loop for i in '("wordnet-package"
		 "wordnet")
    do
      (compile-file (make-pathname :defaults *wordnet-source-dir*
				   :name i
				   :type "lisp")
		    :load-after-compile t))