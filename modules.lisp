(in-package :module-manager)

;;; ===========================================================================
;;;
;;; Author: Raymond de Lacaze
;;;
;;; This file defines the interface to WORDNET using GBBopen's mini-module system.
;;;
;;; ===========================================================================

;;;----------------------------------------------------------------------------
;;; Wordnet Package
;;;----------------------------------------------------------------------------

(eval-when (eval compile load)
  (unless (find-package :wordnet)
    (make-package "WORDNET"
		  :use '(#-SBCL :user #+SBCL :cl-user :common-lisp #-SBCL
			 :clos #-SBCL :excl)
		  :nicknames '(:word))))

;;;----------------------------------------------------------------------------

(define-root-directory '(:wordnet-root) *load-truename*)


;;;----------------------------------------------------------------------------
;;; Wordnet Module
;;;----------------------------------------------------------------------------

(define-module :wordnet
  (:requires :utilities)
  (:directory :wordnet-root)
  (:files "wordnet"
	  "Wordnet-API"))

;;;----------------------------------------------------------------------------
;;; End of File
;;;----------------------------------------------------------------------------
