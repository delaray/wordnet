;;;-*- Mode: Lisp; Package: COMMON-LISP-USER -*-

(defpackage :wordnet (:use #:asdf #:cl) (:nicknames "WORD"))

;;;----------------------------------------------------------------------------
;;; Graph Module System Definition
;;;----------------------------------------------------------------------------

(in-package :wordnet)

(defsystem wordnet
  :author "Raymond de Lacaze <delaray@hotmail.com>"
  :version "1.0"
  :maintainer  "Raymond de Lacaze <delaray@hotmail.com>"
  :licence "MIT Style license for the packaging."
  :description "WordNet3 API Module"
  :long-description  "WordNet3 API Module"
  :depends-on (:utilities)
  :components ((:module "Source"
                        :components ((:file "Wordnet")
				     (:file "Wordnet-API")))))
					    
#+IGNORE
(asdf:oos 'asdf:load-op :wordnet)

;;;----------------------------------------------------------------------------
;;; End of File
;;;----------------------------------------------------------------------------
