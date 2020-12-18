(in-package :wordnet)


;;;*****************************************************************************
;;; WORDNET 3.0
;;;*****************************************************************************
;;;
;;; File Contents
;;; -------------
;;;
;;; *WORDNET-DATA-DIR*
;;; %INITIALIZE-WORDNET
;;; *SYNSET-RELATION-TYPES* 
;;; PTR-SYMBOLS-FROM-STRING
;;;
;;; SYNSET
;;; WORDNET-NOUN
;;; WORDNET-ADJECTIVE
;;; WORDNET-ADVERB
;;; WORDNET-VERB 
;;;
;;; FIND-WORDNET-CLASS-NAME
;;; FIND-SYNSET-POS
;;; WORDNET-PART-OF-SPEECH

;;;*****************************************************************************

;;;----------------------------------------------------------------------------
;;; WordNet Pathname Related Variables
;;;----------------------------------------------------------------------------

(defparameter *WORDNET-DEVICE*
    (pathname-device *load-truename*))

;;;----------------------------------------------------------------------------

(defparameter *WORDNET-DIRECTORY*
   (pathname-directory *load-truename*))

;;;----------------------------------------------------------------------------

(defparameter *WORDNET-ROOT*
    (make-pathname :name nil :type nil
                   :directory (pathname-directory *load-truename*)
                   :defaults  *load-truename*))


;;;---------------------------------------------------------------------------
;;; PATHNAMES
;;;---------------------------------------------------------------------------

(defun MAKE-WORDNET-DATA-PATHNAME ()
  (make-pathname :device (pathname-device *wordnet-root*)
		 :directory '(:absolute "Projects" "WordNet3" "Data")))

;;;---------------------------------------------------------------------------

(defvar *WORDNET-DATA-DIR* (make-wordnet-data-pathname))

;;;--------------------------------------------------------------------------------
;;; %INITIALIZE-WORDNET
;;;--------------------------------------------------------------------------------  

(defun %INITIALIZE-WORDNET()
  (clrhash *wordnet-name-hash*)
  (load-wordnet-data :noun (merge-pathnames "data.noun" *wordnet-data-dir*))
  (load-wordnet-data :verb (merge-pathnames "data.verb" *wordnet-data-dir*))
  (load-wordnet-data :adjective (merge-pathnames "data.adj" *wordnet-data-dir*))
  (load-wordnet-data :adverb (merge-pathnames "data.adv" *wordnet-data-dir*)))

;;;--------------------------------------------------------------------------------
;;; *SYNSET-RELATION-TYPES* 
;;;--------------------------------------------------------------------------------  

(defvar *SYNSET-RELATION-TYPES* 
    '(:Noun (:ANTONYM 
	     :HYPERNYM 
	     :INSTANCE-HYPERNYM 
	     :HYPONYM 
	     :INSTANCE-HYPONYM 
	     :INSTANCE-HYPONYM 
	     :MEMBER-HOLONYM 
	     :SUBSTANCE-HOLONYM 
	     :PART-HOLONYM 
	     :MEMBER-MERONYM 
	     :SUBSTANCE-MERONYM 
	     :PART-MERONYM 
	     :ATTRIBUTE 
	     :DERIVATIONALLY-RELATED-FORM 
	     :DOMAIN-OF-SYNSET-TOPIC 
	     :MEMBER-OF-THIS-DOMAIN-TOPIC 
	     :DOMAIN-OF-SYNSET-REGION 
	     :MEMBER-OF-THIS-DOMAIN-REGION 
	     :DOMAIN-OF-SYNSET-USAGE 
	     :MEMBER-OF-THIS-DOMAIN-USAGE)
      :verb (:ANTONYM
	     :SIMILAR-TO
	     :PARTICIPLE-OF-VERB
	     :PERTAINYM
	     :ATTRIBUTE
	     :ALSO-SEE
	     :DERIVATIONALLY-RELATED-FORM
	     :DOMAIN-OF-SYNSET-TOPIC
	     :DOMAIN-OF-SYNSET-REGION
	     :DOMAIN-OF-SYNSET-USAGE)
      :adjective  (:ANTONYM
		   :SIMILAR-TO
		   :PARTICIPLE-OF-VERB
		   :PERTAINYM
		   :ATTRIBUTE
		   :ALSO-SEE
		   :DERIVATIONALLY-RELATED-FORM
		   :DOMAIN-OF-SYNSET-TOPIC
		   :DOMAIN-OF-SYNSET-REGION
		   :DOMAIN-OF-SYNSET-USAGE)
      :adverb (:ANTONYM 
	       :DERIVED-FROM-ADJECTIVE
	       :DOMAIN-OF-SYNSET-TOPIC
	       :DOMAIN-OF-SYNSET-REGION
	       :DOMAIN-OF-SYNSET-USAGE
	       :DERIVATIONALLY-RELATED-FORM)))

;;;--------------------------------------------------------------------------------  
;;; PTR-SYMBOLS-FROM-STRING
;;;--------------------------------------------------------------------------------  

(defun PTR-SYMBOLS-FROM-STRINGS (pos sym-strings)
  (let ((keyword-package (find-package :keyword)))
    (flet ((lookup (assoc-list)
                   (loop for s in sym-strings
                         collecting (or (cdr (assoc s assoc-list :test 'string=))
                                        (break "unknown ptr sym ~a" s)
                                        (intern s keyword-package)))))
      (case pos
	(:n (lookup '(("!" . :Antonym)
		      ("@" . :Hypernym)
		      ("@i" . :Instance-Hypernym)
		      ("~" . :Hyponym)
		      ("i" . :Instance-Hyponym) ;; I think this is wrong and next is right but cannot hurt?
		      ("~i" . :Instance-Hyponym)
		      ("#m" . :Member-holonym)
		      ("#s" . :Substance-holonym)
		      ("#p" . :Part-holonym)
		      ("%m" . :Member-meronym)
		      ("%s" . :Substance-meronym)
		      ("%p" . :Part-meronym)
		      ("=" . :Attribute)
		      ("+" . :Derivationally-related-form)
		      (";c" . :Domain-of-synset-TOPIC)
		      ("-c" . :Member-of-this-domain-TOPIC)
		      (";r" . :Domain-of-synset-REGION)
		      ("-r" . :Member-of-this-domain-REGION)
		      (";u" . :Domain-of-synset-USAGE)
		      ("-u" . :Member-of-this-domain-USAGE ))))
	(:v (lookup '(("!" . :Antonym)
		      ("@" . :Hypernym)
		      ("~" . :Hyponym)
		      ("*" . :entailment)                 
		      (">" . :cause)
		      ("^" . :also-see)
		      ("$" . :verb-group)
		      ("+" . :Derivationally-related-form)
		      (";c" . :Domain-of-synset-TOPIC)
		      (";r" . :Domain-of-synset-REGION)
		      (";u" . :Domain-of-synset-USAGE))))
	((:a :s) ;; s is adjective satellite
	 (lookup '(("!" . :Antonym)
		   ("&" . :similar-to)
		   ("<" . :participle-of-verb)
		   ("\\" . :pertainym)                 
		   ("=" . :attribute)
		   ("^" . :also-see)
		   ("+" . :Derivationally-related-form)
		   (";c" . :Domain-of-synset-TOPIC)
		   (";r" . :Domain-of-synset-REGION)
		   (";u" . :Domain-of-synset-USAGE))))
	(:r (lookup '(("!" . :Antonym)
		      ("\\" . :derived-from-adjective)                 
		      (";c" . :Domain-of-synset-TOPIC)
		      (";r" . :Domain-of-synset-REGION)
		      (";u" . :Domain-of-synset-USAGE)
		      ("+" . :Derivationally-related-form))))
	(otherwise
	 (break "ptr-symbols-from-strings> unknown POS ~a for syms ~a" pos sym-strings))))))

;;;--------------------------------------------------------------------------------  

(defun GLOSS-PARSE (gloss)
  (loop for seg in (util::split-sequence #\; gloss)
      collecting (string-trim " \"" seg)))

;;;************************************************************************
;;; GENERICS
;;;************************************************************************

(defgeneric FIND-SYNSET-POS (class))

(defgeneric SYNSET-P (class))

;;;************************************************************************
;;; WORDNET CLASS DEFINITIONS
;;;************************************************************************

(defclass SYNSET ()
  (;; Used in loading. Only unique per POS.
   (index :initarg :index :accessor get-index)
   (words :initarg :words :accessor get-words)
   (related-synsets :initarg :related-synsets :accessor get-related-synsets)
   (definition :initarg :definition :accessor get-definition)))

(defmethod SYNSET-P ((obj SYNSET))
  t)

(defmethod SYNSET-P ((obj t))
  nil)

(defmethod PRINT-OBJECT ((self synset) stream)
  (format stream "<~a: ~s>" (type-of self) (get-words self)))

(defclass WORDNET-NOUN (synset)
  ())

(defclass WORDNET-ADJECTIVE (synset)
  ())

(defclass WORDNET-ADVERB (synset)
  ())

(defclass WORDNET-VERB (synset)
  ())

;; we get n, v, a, r from wordnet files directly, so support those
;; also support more mneumonic keywords (:verb, :noun, etc)

;;;------------------------------------------------------------------------------------
;;; FIND-WORDNET-CLASS-NAME
;;;------------------------------------------------------------------------------------

(defun FIND-WORDNET-CLASS-NAME (pos)
  (ecase pos
    ((n :noun) 'wordnet-noun)
    ((v :verb) 'wordnet-verb)
    ((a :adjective) 'wordnet-adjective)
    ((r :adverb) 'wordnet-adverb)))

;;;------------------------------------------------------------------------------------
;;; FIND-SYNSET-POS
;;;------------------------------------------------------------------------------------

;;; The reverse operation: given a synset, return the POS.  2/17/08

(defmethod FIND-SYNSET-POS ((obj wordnet-noun)) :noun)
(defmethod FIND-SYNSET-POS ((obj wordnet-adjective)) :adjective)
(defmethod FIND-SYNSET-POS ((obj wordnet-verb)) :verb)
(defmethod FIND-SYNSET-POS ((obj wordnet-adverb)) :adverb)

;;;------------------------------------------------------------------------------------
;;; WORDNET-PART-OF-SPEECH
;;;------------------------------------------------------------------------------------

(defun WORDNET-PART-OF-SPEECH (word)
  "Return a list of part-of-speech symbols for the given word, if known.  Nil if unknown."
  (let ((word-def (lookup-word word))
	(results '()))
    (when word-def
      (dolist (def word-def)
	(pushnew (find-synset-pos def) results)))
      results))


;;;====================================================================================
;;; don't seem to need index files...keep for reference or future use
;;;====================================================================================

#+ignore(defvar *wordnet-index* nil)

#+ignore
(defun load-wordnet-index (dictionary-path)
  (with-open-file (file dictionary-path :direction :input)
    (loop with item = (make-string 1024)
        for (item-len reason) = (multiple-value-list (excl:read-line-into item file nil :eof))
        for line-no upfrom 0
        for in-copyright = t then (and in-copyright (char= #\space (schar item 0)))
        until (eq reason :eof)
        unless in-copyright do
          (destructuring-bind (lemma  pos$  synset-cnt  p-cnt$ &rest more)
              (util::split-sequence #\space item :end item-len)
            (let ((p-cnt (parse-integer p-cnt$))
                  (pos (intern (string-upcase pos$)
			       (find-package :keyword))))
              (destructuring-bind (sense-cnt tagsense-cnt &rest synset-offsets)
                  (subseq more p-cnt)
                (declare (ignorable tagsense-cnt sense-cnt))
                #+shh (break "stroing ~a ~a ~a"
			     (nsubstitute #\space #\_ lemma) synset-offsets
			     (mapcar 'parse-integer (subseq synset-offsets 0 (parse-integer synset-cnt))))

		(push (list :pos pos
			    :lemma (nsubstitute #\space #\_ lemma) 
			    :synsets
			    (mapcar 'parse-integer (subseq synset-offsets 0
							   (parse-integer synset-cnt))))
		      *wordnet-index*))))
	  )))

;;;====================================================================================
;;; Wordnet data
;;;====================================================================================

(defparameter *wordnet-hash-size* 400000)

(defvar *wordnet-name-hash* (make-hash-table :size *wordnet-hash-size* :test #'equalp))
(defun lookup-word (string &key part-of-speech)
  "return list of possible synsets for given string"
  (assert (member part-of-speech '(:noun :verb :adjective :adverb nil)))
  (if part-of-speech
    (loop for i in (gethash string *wordnet-name-hash*)
	when (eq (find-synset-pos i) part-of-speech)
	collect i)
    (gethash string *wordnet-name-hash*)))

(defvar *wordnet-index-hash* (make-hash-table :size *wordnet-hash-size*))
(defun lookup-wordnet-index (index)
  (gethash index *wordnet-index-hash*))

(defvar *wordnet-data* nil) ;; keep raw data around globally for now

;; Modified from old version. 
(defun load-wordnet-data (pos dictionary-path)
  (setq *wordnet-data* nil)
  (clrhash *wordnet-index-hash*)

  (with-open-file (file dictionary-path :direction :input)
     (loop with keyword-package = (find-package :keyword)
	   for item = (read-line file nil :eof)
	   for item-len = (unless (eq item :eof)
			    (length item))
	   for line-no upfrom 0
                
	   for in-copyright = t then (and in-copyright (char= #\space (schar item 0)))
	   until (eq item :eof)
	   unless in-copyright do
	   (destructuring-bind (synset-offset lex-filenum  ss-pos  w-cnt &rest more)
	       (util::split-sequence #\space item :end (position #\| item :end item-len))
	     (declare (ignore lex-filenum))
	     (let* ((w-cnt (parse-integer w-cnt :radix 16))
                   
		    (words (loop repeat w-cnt
				 for (w) on more by #'cddr
				 collecting w)))
	       (destructuring-bind (p-cnt &rest ptr-info)
		   (subseq more (* 2 w-cnt))
		 (let* ((p-cnt (parse-integer p-cnt))
			(related-synsets
			 (loop repeat p-cnt
			       for (pointer-symbol synset-offset pos nil) on ptr-info by #'cddddr
			       unless (string= synset-offset "")
			       collecting (let ((rel-type (car (ptr-symbols-from-strings 
								(intern (string-upcase ss-pos) keyword-package)
								(list pointer-symbol)))))
					    (list rel-type (parse-integer synset-offset)
						  (intern (string-upcase pos)
							  keyword-package)))))
			(glossaries (let ((bar-pos (position #\| item :end item-len)))
				      (when bar-pos
					(gloss-parse (string-trim " " (subseq item (1+ bar-pos) item-len)))))))
		   (let* ((index (parse-integer synset-offset))
			  (synset (make-instance (find-wordnet-class-name pos)
						 :index index
						 :words (loop for i in words
							      collect (nsubstitute #\space #\_ i))
						 :related-synsets related-synsets
						 :definition (car glossaries))))
		     (dolist (word (get-words synset))
		       (push synset
			     (gethash word *wordnet-name-hash*)))
		     (when (gethash index *wordnet-index-hash*)
		       (error "index ~a already used! Can't add ~a" 
			      index synset))
		     (setf (gethash index *wordnet-index-hash*)
			   synset)
		     (push synset *wordnet-data*)
		     )
		   ))))) ;; link synsets together using stored indices
    (linkup-wordnet-data)
    ))


;;;---------------------------------------------------------------------------------------
;;; LINKUP-WORDNET-DATA
;;;---------------------------------------------------------------------------------------

(defun LINKUP-WORDNET-DATA ()
  (dolist (e *wordnet-data*)
    (setf (get-related-synsets e)
      (loop for (relation index pos) in (get-related-synsets e)
	  for synset = (lookup-wordnet-index index)
	  when synset
	  collect (progn
		    pos ;; ignore this
		    (assert relation () "missing relation in synsets ~a in index ~a" (get-related-synsets e) e)
		    (list relation synset)
		    ;; (make-instance relation :synset synset)
		    )))))

;;;---------------------------------------------------------------------------------------
;;; FIND-RELATION-COST
;;;---------------------------------------------------------------------------------------

;; dummy function for now...need to understand better what's needed before 
;; going down any particular road here

(defun FIND-RELATION-COST (relation)
  (case relation
    (:MEMBER-OF-THIS-DOMAIN-REGION 5)    
    (:DOMAIN-OF-SYNSET-USAGE 5)
    (otherwise 5)))


;;;---------------------------------------------------------------------------------------
;;;  FIND-WORDNET-PATH
;;;---------------------------------------------------------------------------------------

;; Todo: allow w1, w2 to be lists of synsets?
;;'(:MEMBER-OF-THIS-DOMAIN-REGION)))

(defun FIND-WORDNET-PATH (w1 w2 &key (max-depth 5) (exclude-relations nil))
  "Find path between words or synsets. w1, w2 are words or synsets"
  (assert (and (or (stringp w1)
		   (synset-p w1))
	       (or (stringp w2)
		   (synset-p w2))))
  (let* ((s1 (if (synset-p w1) (list w1) (lookup-word w1)))
	 (s2 (if (synset-p w2) (list w2) (lookup-word w2)))
	 (frontier-hash (make-hash-table))
	 (frontier s1))
    (when (or (not s1) (not s2))
      (error "Couldn't find word for ~a" (if (not s1) w1 w2)))
    (loop repeat max-depth
	do (loop for i in frontier
	       for from-entry = (gethash i frontier-hash)
	       for from-last = (first from-entry)
	       for from-cost = (or (getf (cddr from-last) :cost)
				   0)
	       do 
		 (loop for (relation related) in (get-related-synsets i)
		     for rel-cost = (find-relation-cost relation)
		     for found = (gethash related frontier-hash)
		     for found-existing-cost = (or (getf (cddr from-last) :cost)
						   0)
		     do
		       (when (and (not (member relation exclude-relations))
				  (or (not found)
				      (> found-existing-cost (+ rel-cost from-cost))))
			 (when (and (not found)
				    (not (gethash related frontier-hash)))
			   ;; add it to frontier list
			   (push related frontier))
			 (setf (gethash related frontier-hash)
			   (cons (list relation related :cost (+ rel-cost from-cost))
				 from-entry))))))
    (remove nil
	    (loop for i in s2
		collect (gethash i frontier-hash)))
    ))

;;;---------------------------------------------------------------------------------------
;;; End of File
;;;---------------------------------------------------------------------------------------
