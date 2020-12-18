(in-package :common-lisp-user)

;;;;***************************************************************************
;;;; WordNet REPL Commands
;;;;***************************************************************************
;;;
;;;  Preparations:
;;;    1. Create gbbopen-modules directory in your (user-homedir-pathname) 
;;;       directory
;;;    2. Create a symbolic link to your system source tree in this
;;;       gbbopen-modules directory (Windows users must create a 
;;;       "pseudo-symbolic-link" file, a text file of type .sym that contains 
;;;       the target directory path as the sole line in the file)
;;;
;;;  Then load GBBopen's initiate.lisp from whatever GBBopen installation is
;;;  desired (which will load this command file).
;;;
;;;  Recommended: Set up your CL environment according to the "Enhancing Your
;;;               Development Environment" exercise in the GBBopen Tutorial.
;;;
;;;;***************************************************************************

;;;   Supporting Entities for WordNet Commands

;; Remember this file:

(defparameter *wordnet-commands-file* *load-truename*)


;;; ===========================================================================

;;;   Useful KB Commands

(with-system-name (:WORDNET)
  
  ;;---------------------------------------------------------------------------

  (define-repl-command :wordnet (&rest options)
    "Load (compile if needed) :wordnet module."
    (format t "~&;; ***** Compiling/Loading Wordnet~%")
    ;; Load/compile code.
    (startup-module :wordnet options :wordnet))
  
;;; -------------------------------------------------------------------------

  )

;;;------------------------------------------------------------------------
;;; End of File
;;;------------------------------------------------------------------------
