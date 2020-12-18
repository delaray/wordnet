(in-package :wordnet)

(def-db ghx-reference *cleaner-data-directory*)


#+test
(ghx-reference-build)

(defun ghx-reference-build (&key (force-p t))
  ;; 11/21/07 MRH: By default, if this function is called, force the re-creation
  ;;               of the WordNet AC-DB.  But, if called from some initialization
  ;;               function, only create if it doesn't exist.
  (when (or force-p
	    (not (ghx-reference-exists-p)))
    (let ((*print-readably* nil)
	  (*package* (find-package :wordnet)))
      (find-map-reset)
      (word-net-reset)
      (when (and *allegrocache*
		 (database-open-p *allegrocache*)
		 (not (eq *allegrocache* pq::*pq-db*)))
	(close-database))
      (ghx-reference-create)
      (master-noun-type-load)
      (ghx-vendor-load)
      (word-abbreviation-load)
      (word-net-load))))

#+lookup
(progn
  (ghx-reference-open-ensure)
  (word-net-reset)
  (inspect (word-net-lookup "dog")))

#+zap
(ghx-reference-build)

;; Ray's API

(defun find-word-definitions (word &key noun verb adjective adverb)
  "Arguments:
<word>: A CL string or symbol.
<parts-of-speech>:	:noun, :verb, :adjective, and :adverb. Controls which
                    parts of speech are are returned.
Returns: A list of definitions for <word> in form (<part-of-speech> <defintion>+)*. 
e.g. for \"dog\", ((:noun \"mammal\" \"poorly performing stock\")(:verb \"pursue\"))."
  (loop with all = (null (or noun verb adjective adverb))
      for wx in (word-net-lookup (down$ word))
      when (or all (ecase (pos-abbr wx) (n noun)(v verb)(a adjective)(r adverb)))
      collect (cons (ecase (pos-abbr wx) (n :noun)(v :verb)(a :adjective)(r :adverb))
                (mapcar 'definition (synsets wx)))))

(defun word-in-dictionary-p (word)
  (word-net-lookup? (down$ word)) )

;;; 2/8/08 MRH: Adding for easy combination with other non-WordNet dictionary methods.
(defun FIND-PART-OF-SPEECH (word)
  "Return a list of part-of-speech symbols for the given word, if known.  Nil if unknown."
  (flet ((wordnet-pos-to-pq (w-pos)
	   (case (word-net-pos-long w-pos)
	     (noun :noun)
	     (verb :verb)
	     (adjective :adjective)
	     (adverb :adverb)
	     (t w-pos))))
     (let ((word-def (word-net-lookup (down$ word))))
       (when word-def
	 (mapcar #'wordnet-pos-to-pq (mapcar #'pos-abbr word-def))))))
	   
#+test
(list 
 (find-word-definitions "dog")
 (find-word-definitions 'dog :verb t))
;;;((VERB "go after with the intent to catch")
;;; (NOUN
;;;  "a member of the genus Canis (probably descended from the common wolf) that has been domesticated by man since prehistoric times"
;;;  "a dull unattractive unpleasant girl or woman" "informal term for a man"
;;;  "someone who is morally reprehensible"
;;;  "a smooth-textured sausage of minced beef or pork usually smoked"
;;;  "a hinged catch that fits into a notch of a ratchet to move a wheel forward or prevent it from moving backward"
;;;  "metal supports for logs in a fireplace"))

(defun find-path-between-words (from-word to-word &key (max-search-depth 5) ) 
"Arguments:
    <from-word>: 		A CL string or symbol for starting point of WordNet search.
    <to-word>: 		A CL string or symbol for end point of search.
Returns: A list of WordNet relations/words formins shortest path 
         from <from-word> to <to-word>."
  (bwhen (path (car (wn-connect-a-dot (down$ from-word) (down$ to-word) max-search-depth)))    
    (loop with fifo
        for ss-node = path then (when (typep ss-node 'ss-search-node)
                                  (ss-node-reached-from ss-node))
        while ss-node
          
        do ;;(describe ss-node)
          ;(print (list (type-of ss-node)))
          ;(print (list (type-of ss-node) (ss-node-value ss-node)))
          ;(print :pushing)
          (etypecase ss-node
            (ss-search-node
             (let ((v (ss-node-value ss-node)))
               (push (list (if (typep v '(or word-net-synset word-net-index-entry))
                               (word-net-pos-long (pos-abbr v))
                             (type-of v))
                       ;;v
                       (etypecase v
                         (word-net-index-entry (lemma v))
                         (word-net-synset (car (words v)))
                         (word-net-relation (car (words (synset v))))))
                 fifo)))
            (word-net-index-entry
             (push (list (word-net-pos-long (pos-abbr ss-node))
                     (lemma ss-node))
               fifo)))
          ;(print :pushed)
        finally ;(print :returning)
          (return fifo))))
          
(defun dog-to-mammal ()
  (ghx-reference-open-ensure)
  (find-map-reset)
  (word-net-reset)
  (with-metrics (t t "dog2mammal")
    (find-path-between-words 'dog "mammal")))
;;;=>
;;;((NOUN "dog")
;;; (HYPERNYM "canine")
;;; (HYPERNYM "carnivore")
;;; (HYPERNYM "placental")
;;; (HYPERNYM "mammal")
;;; (NOUN "mammal"))


;;; --- abbreviations ---------------------------------------------------------

(define-symbol-macro *ghx-word-abbreviations-path*
  (merge-pathnames "GHX-Standard-Abbreviations.lisp" *cleaner-initial-resource-directory*))

#+boom
(word-abbreviation-load)

(defun word-abbreviation-load ()
  (ghx-reference-open-ensure)

  (with-db (*ghx-reference* :commit t)
    (loop for mn in '("word-abbreviation" "abbreviation-word")
      do (db-map-delete mn)
          (db-map-create mn))

    (with-open-file (file *ghx-word-abbreviations-path* :direction :input)
      (loop with wa = (find-map "word-abbreviation")
          and aw = (find-map "abbreviation-word")
          for word-abbr in (read file)
          for (word$ abbr$) = (mapcar 'princ-to-string word-abbr)
          do (pushnew abbr$ (map-value wa word$) :test 'string-equal)
            (pushnew word$ (map-value aw abbr$) :test 'string-equal)))))

(defun ghx-vendor-load ()
  (ghx-reference-open-ensure)
  
  (with-db (*ghx-reference* :commit t)
    (db-map-delete "ghx-vendor")
    (with-open-file (file (merge-pathnames "ghx-vendor.csv"
                            *cleaner-initial-resource-directory*)
                      :direction :input)
      (loop initially (read-line file)
          with gv = (db-map-create "ghx-vendor")
          for gv-line = (read-line file nil nil)
          while gv-line do
            (let ((vname (string-trim '(#\") (second (split-sequence #\tab gv-line)))))
              (setf (map-value gv vname) t))
          finally
            (commit)))))


#+test
(map-map* (find-map "abbreviation-word")
  (lambda (k v)
    (when (> (length v) 1)
      (print (list k v))
      (break))))

(defun unabbreviate (abbreviation)
  "Input must be in uppercase"
  (with-db (*ghx-reference*)
    (map-value (find-map "abbreviation-word") abbreviation)))

#+test
(progn
  (close-database :db *ghx-reference*)
  (ghx-reference-open-ensure)
  (with-db (*ghx-reference*)
    (find-map "abbreviation-word")))

(defun abbreviate (word)
  "Input must be in uppercase"
  (with-db (*ghx-reference*)
    (car (map-value (find-map "word-abbreviation") word))))

#+test
(unabbreviate "PKG")


#+test
(abbreviate "CUVETTES")

#+test
(singularify "cuvettes")

#+nil
(defun do-absorb ()
  (print `(absorb defined as ,(ghx-definitions "absorb"))))

#+test
(word-net-lookup "aortic")

(defun define-joint ()
  (loop for (sense . defs) in (ghx-definitions "joint")
        do (loop for d in defs do
                 (trcx "joint" sense d))))

(defun ghx-test ()
  (test-setup)

  ; #+nahh 
  (loop for rt in '(sleeve plus latex legend manifold rapid )
      do (trc "ghx:" rt (ghx-definitions (down$ rt)))))

; above using maps
; cpu time (non-gc) 1,172 msec user, 0 msec system
; cpu time (gc)     32 msec user, 0 msec system
; cpu time (total)  1,204 msec user, 0 msec system
; real time  1,203 msec
; space allocation:
;  478,481 cons cells, 24,280,400 other bytes, 0 static bytes

(defstruct (wn-info (:type list))
  pos types offsets)

#+test
(ghx-definitions "plate")

(defparameter +word-net-down-concepts+
  '(hyponym similar-to also-see
     member-of-this-domain-topic
     member-of-this-domain-usage
     domain-of-synset-usage
     |~i| instance-hyponym ;; same until new load
     instance-hypernym 
     member-meronym meronym part-meronym
     member-holonym
     derivationally-related-form
     substance-meronym
     part-meronym
     part-holonym
     antonym))

(defun ghx-definitions (word$ &optional verbose)
  (bwhen (s (singularify-ex (down$ word$)))
    (setf word$ s))
  (bwhen (senses (word-net-lookup word$))
    (if (and (null (cdr senses))
          (null (cdr (synsets (car senses)))))
        (eko ("ghxdef short list" word$)
          (list (list (type-of (car senses))
                  (definition (car (synsets (car senses)))))))
      (loop for sense in senses
          for ghx-defs = (loop for synset in (synsets sense)
                             for path = (or
                                         ;;-- human body ---
                                         (or (word-path-try synset 'word-net-noun-synset 05217168  
                                               nil +word-net-down-concepts+ 5)
                                           ;;-- body part ---
                                           (word-path-try synset 'word-net-noun-synset 5220461
                                             nil +word-net-down-concepts+ 5))

                                         #+not 
                                         (loop for (to-pos to-offset)
                                             in '((word-net-noun-synset 6084469) ; chemistry
                                                  )
                                             thereis (word-path-try synset to-pos to-offset
                                                       nil +word-net-down-concepts+ 4))

                                         (loop for (to-pos to-offset)
                                             in '((word-net-noun-synset 6084469) ; chemistry
                                                  (word-net-noun-synset 6043075) ; medicine
                                                  (word-net-noun-synset 6037666) ; biology
                                                  ;(word-net-noun-synset 13526110); organic process - picks up oral sex for "Head"
                                                  (word-net-noun-synset 3183080) ; device
                                                  (word-net-noun-synset 4341414) ; structural_member
                                                  (word-net-noun-synset 3964744) ; -- plaything -- only way to get balloon!
                                                  (word-net-noun-synset 13583724) ; unit of measure
                                                  (word-net-noun-synset 1014066) ; -- collection (gathering)
                                                  (word-net-noun-synset 7951464) ; -- collection (several things) for set
                                                  (word-net-noun-synset 4451818) ; tool
                                                  (word-net-noun-synset 321956) ; enclosure
                                                  (word-net-adjective-synset 65064) ; plus (for plus :{)
                                                  (word-net-noun-synset 3058107) ; coating (for latex)
                                                  (word-net-noun-synset 13901321) ; pipe (for sleeve)
                                                  (word-net-noun-synset 13815152) ; magnitude_relation (odd way to get rapid)
                                                  (word-net-noun-synset 33020) ;  communication
                                                  (word-net-noun-synset 3309808) ; fabric, for cotton
                                                  )
                                             thereis (word-path-try synset to-pos to-offset
                                                       nil +word-net-down-concepts+ 4)))
                             if path
                             collect (progn
                                       (trc verbose "bingo sense" synset (left$ (definition synset) 20))
                                       (when verbose ;; (search "witty" (definition synset))
                                         (trc "path for above follows, length:" (length path))
                                         (loop for p in path do
                                               (trc "step" p)))
                                       (definition synset))
                               ;;else do (trcx sense-less pos offset)
                               )
          when ghx-defs
          collect (cons (type-of sense) ghx-defs)))))


