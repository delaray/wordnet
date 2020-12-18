(in-package :wordnet)

;;;*****************************************************************************
;;; WORDNET 3.0 API
;;;*****************************************************************************
;;;
;;; File Contents
;;; -------------
;;;
;;; *WORDNET-INITIALIZED-P*
;;; INITIALIZE-WORDNET
;;;
;;; FIND-WORDNET-WORD
;;; FIND-PART-OF-SPEECH
;;; FIND-WORD-DEFINITIONS
;;;
;;; WORDNET-HYPERNYMS
;;; WORDNET-HYPONYMS
;;; WORDNET-SYNONYMS
;;;
;;; WORDNET-NOUN-P
;;; WORDNET-ADJECTIVE-P
;;; WORDNET-VERB-P
;;; WORDNET-ADVERB-P
;;; 
;;;*****************************************************************************

;;;-----------------------------------------------------------------------------
;;; *WORDNET-INITIALIZED-P*
;;;-----------------------------------------------------------------------------

(defvar *WORDNET-INITIALIZED-P* nil)

;;;-----------------------------------------------------------------------------
;;; INITIALIZE-WORDNET
;;;-----------------------------------------------------------------------------

(defun INITIALIZE-WORDNET ()
  (when (not *wordnet-initialized-p*)
    (format t "~&Initializing WORDNET 3.0...")
    (%initialize-wordnet)
    (setf *wordnet-initialized-p* t)))

;;;----------------------------------------------------------------------
;;; FIND-WORDNET-WORD 
;;;----------------------------------------------------------------------

(defun %FIND-WORDNET-WORD (word)
  (lookup-word word))

;;;----------------------------------------------------------------------

(defmethod FIND-WORDNET-WORD ((word SYMBOL) &rest rest)
  (apply 'find-wordnet-word  (symbol-name word) rest))

;;;----------------------------------------------------------------------

(defmethod FIND-WORDNET-WORD ((word STRING) &rest rest)
  (cond ((util::alpha-string-hyphenated-p word)
	 (or (apply #'%find-wordnet-word word rest)
	     (and (every #'(lambda (x)
			     (apply #'%word-in-wordnet-p x rest))
			 (util::split-sequence #\- word))
		  (find-wordnet-word (first (last (util::split-sequence #\- word)))))))
	(t
	 (apply #'%find-wordnet-word word rest))))

;;;----------------------------------------------------------------------
;;; FIND-PART-OF-SPEECH
;;;----------------------------------------------------------------------
	      
(defun FIND-PART-OF-SPEECH (word)
  (%find-part-of-speech word))

;;;----------------------------------------------------------------------
;;; WORDNET-HYPERNYMS
;;;----------------------------------------------------------------------

(defmethod WORDNET-HYPERNYMS ((word SYNSET))
  (%wordnet-nyms word :hypernym))

;;;----------------------------------------------------------------------
;;; WORDNET-HYPONYMS
;;;----------------------------------------------------------------------

(defmethod WORDNET-HYPONYMS ((word SYNSET))
  (%wordnet-nyms word :hyponym))

;;;----------------------------------------------------------------------
;;; WORDNET-HOLONYMS
;;;----------------------------------------------------------------------

(defmethod WORDNET-HOLONYMS ((word SYNSET))
  (%wordnet-nyms word :holonym))

;;;----------------------------------------------------------------------
;;; WORDNET-MERONYMS
;;;----------------------------------------------------------------------

(defmethod WORDNET-MERONYMS ((word SYNSET))
  (%wordnet-nyms word :meronym))

;;;----------------------------------------------------------------------
;;; %WORDNET-NYMS
;;;----------------------------------------------------------------------

(defmethod %WORDNET-NYMS ((word SYNSET)(nym SYMBOL))
  (let ((results nil))
    (dolist (entry (get-related-synsets word))
      (when (or (eq (first entry) nym)
		(eq (first entry)
		    (intern (concatenate 'STRING "PART-" (symbol-name nym)) :keyword)))
	(push (second entry) results)))
    (nreverse results)))

;;;----------------------------------------------------------------------
;;; WORDNET-SYNONYMS
;;;----------------------------------------------------------------------

(defmethod WORDNET-SYNONYMS ((word-string STRING))
  (let* ((word (find-wordnet-word word-string))
	 (words (mapcar #'get-words word))
	 (result nil))
    (dolist (word-list words)
      (setf result 
	(append (remove word-string word-list :test #'string-equal)
		result)))
    (nreverse result)))

;;;----------------------------------------------------------------------
;;; FIND-WORD-DEFINITIONS
;;;----------------------------------------------------------------------

;;; Returns a list of wordnet definitions partitioned by part of
;;; speech. Each entry isof theform:

;;; (<part-of-speech> <def1><def2>...)

(defmethod FIND-WORD-DEFINITIONS ((word SYMBOL))
  (find-word-definitions (symbol-name word)))

;;;----------------------------------------------------------------------

(defmethod FIND-WORD-DEFINITIONS ((word STRING))
  (flet ((format-definition (def)
	   ;; format new WordNet definition to have same form as old
	   (cons (find-synset-pos def)
		 (get-definition def))))
  (let* ((definitions (lookup-word word))
	 (new-definitions nil))
    ;; return collected definitions by part of speech
    (dolist (def definitions)
      (let* ((new-def (format-definition def))
	     (pos-list (assoc (car new-def) new-definitions)))
	(if (null pos-list)
	  (push (cons (car new-def) (list (cdr new-def))) new-definitions)
	  (push (cdr new-def) (cdr pos-list)))))
    new-definitions)))

;;;-----------------------------------------------------------------------------
;;; WORDNET-NOUN-P
;;;-----------------------------------------------------------------------------

(defmethod WORDNET-NOUN-P ((word STRING))
  (when (member :noun (find-part-of-speech word) :test #'eq)
    t))

;;;-----------------------------------------------------------------------------

;;; Do not change this definition without talking to Ray. Thank you.

(defmethod WORDNET-NOUN-P ((word SYMBOL))
  (wordnet-noun-p (symbol-name word)))

;;;-----------------------------------------------------------------------------
;;; WORDNET-ADJECTIVE-P
;;;-----------------------------------------------------------------------------

(defmethod WORDNET-ADJECTIVE-P ((word STRING))
  (when (member :adjective (find-part-of-speech word) :test #'eq)
    t))

;;;-----------------------------------------------------------------------------

(defmethod WORDNET-ADJECTIVE-P ((word SYMBOL))
  (wordnet-adjective-p (symbol-name word)))
  

;;;-----------------------------------------------------------------------------
;;; WORDNET-VERB-P
;;;-----------------------------------------------------------------------------

(defmethod WORDNET-VERB-P ((word STRING))
  (when (member :verb (find-part-of-speech word) :test #'eq)
    t))

;;;-----------------------------------------------------------------------------

(defmethod WORDNET-VERB-P ((word SYMBOL))
  (wordnet-verb-p (symbol-name word)))  

;;;-----------------------------------------------------------------------------
;;; WORDNET-ADVERB-P
;;;-----------------------------------------------------------------------------

(defmethod WORDNET-ADVERB-P ((word STRING))
  (when (member :adverb (find-part-of-speech word) :test #'eq)
    t))  

;;;-----------------------------------------------------------------------------

(defmethod WORDNET-ADVERB-P ((word SYMBOL))
  (wordnet-adverb-p (symbol-name word)))

;;;-----------------------------------------------------------------------------
;;; End of File
;;;-----------------------------------------------------------------------------
