(in-package :wordnet)

;;; --- word net -----------------------------------
;;;
;;;
;;
(defclass* word-net-index-entry (:print nil)
  (lemma :index :any)
  synsets)

(defmethod print-object ((self word-net-index-entry) s)
  (format s "IX:~a:~a" (pos-abbr self) (lemma self)))

(defclass* word-net-synset (:print nil)
  (offset :index :any)
  words
  related-synsets
  definition)

#+test
(progn
  (ghx-reference-open-ensure)
  (with-db (*ghx-reference*)
    (doclass* (ss 'word-net-synset)
      (loop for rss in (related-synsets ss)
          ;;for x below 10
          do (assert (typep rss 'word-net-relation))
            ))))

#+test
(let ((n 0))
  (ghx-reference-open-ensure)
  (with-db (*ghx-reference*)
    (doclass* (ss 'word-net-synset)
      (print ss)
      (when (> (incf n) 10)(break)))))

(defmethod print-object ((self word-net-synset) s)
  (format s "SS:~a:~a" (pos-abbr self) (car (words self))))

(defclass* word-net-relation (:print nil)
  synset)

(defmethod print-object ((self word-net-relation) s)
  (format s "RSS:~a:~a" (type-of self) (synset self)))

(defclass* word-net-noun (word-net-index-entry :print nil)
  (pos-abbr :initform 'N :allocation :class))
(defclass* word-net-noun-synset (word-net-synset :print nil)
  (pos-abbr :initform 'N :allocation :class))
(DEFMETHOD synset-class ((self word-net-noun))
  'word-net-noun-synset)

(defclass* word-net-verb (word-net-index-entry :print nil)
  (pos-abbr :initform 'V :allocation :class))
(defclass* word-net-verb-synset (word-net-synset :print nil)
  (pos-abbr :initform 'V :allocation :class))
(DEFMETHOD synset-class ((self word-net-verb))
  'word-net-verb-synset)

(defclass* word-net-adverb (word-net-index-entry :print nil)
  (pos-abbr :initform 'R :allocation :class))
(defclass* word-net-adverb-synset (word-net-synset :print nil)
  (pos-abbr :initform 'R :allocation :class))
(DEFMETHOD synset-class ((self word-net-adverb))
  'word-net-adverb-synset)

(defclass* word-net-adjective (word-net-index-entry :print nil)
  (pos-abbr :initform 'A :allocation :class))
(defclass* word-net-adjective-synset (word-net-synset :print nil)
  (pos-abbr :initform 'A :allocation :class))
(DEFMETHOD synset-class ((self word-net-adjective))
  'word-net-adjective-synset)

(defun word-net-pos-to-index-class-name (pos)
  (ecase pos
    (n 'word-net-noun)
    (v 'word-net-verb)
    (a 'word-net-adjective)
    (r 'word-net-adverb)))

(defun word-net-pos-long (pos)
  (ecase pos
    (n 'noun)
    (v 'verb)
    (a 'adjective)
    (r 'adverb)))

(defun word-net-pos-to-synset-class-name (pos)
  (ecase pos
    ((n :noun) 'word-net-noun-synset)
    ((v :verb) 'word-net-verb-synset)
    ((a :adjective :adj) 'word-net-adjective-synset)
    ((r :adverb :adv) 'word-net-adverb-synset)))



(defmacro def-word-net-relations (&rest pos-relcode-relations)
  (let (seen-relations)
    `(PROGN
       ,@(loop for (nil . relcode-relations) in pos-relcode-relations
            nconcing (loop for (nil . relation) in relcode-relations
                         unless (find relation seen-relations)
                         collect (progn
                                   (push relation seen-relations)
                                   `(defclass* ,relation (word-net-relation :print nil)())))))))
(def-word-net-relations
    (n ("!" . Antonym)
      ("@" . Hypernym)
      ("@i" . Instance-Hypernym)
      ("~" . Hyponym)
      ("i" . Instance-Hyponym) ;; I think this is wrong and next is right but cannot hurt?
      ("~i" . Instance-Hyponym)
      ("#m" . Member-holonym)
      ("#s" . Substance-holonym)
      ("#p" . Part-holonym)
      ("%m" . Member-meronym)
      ("%s" . Substance-meronym)
      ("%p" . Part-meronym)
      ("=" . Attribute)
      ("+" . Derivationally-related-form)
      (";c" . Domain-of-synset-TOPIC) 
      ("-c" . Member-of-this-domain-TOPIC) 
      (";r" . Domain-of-synset-REGION)
      ("-r" . Member-of-this-domain-REGION)
      (";u" . Domain-of-synset-USAGE)
      ("-u" . Member-of-this-domain-USAGE ))
    (v ("!" . Antonym)
      ("@" . Hypernym)
      ("~" . Hyponym)
      ("*" . entailment)                 
      (">" . cause)
      ("^" . also-see)
      ("$" . verb-group)
      ("+" . Derivationally-related-form)
      (";c" . Domain-of-synset-TOPIC)
      (";r" . Domain-of-synset-REGION)
      (";u" . Domain-of-synset-USAGE))
  (a ("!" . Antonym)
    ("&" . similar-to)
    ("<" . participle-of-verb)
    ("\\" . pertainym)                 
    ("=" . attribute)
    ("^" . also-see)
    ("+" . Derivationally-related-form)
    (";c" . Domain-of-synset-TOPIC)
    (";r" . Domain-of-synset-REGION)
    (";u" . Domain-of-synset-USAGE))
  (r ("!" . Antonym)
    ("\\" . derived-from-adjective)                 
    (";c" . Domain-of-synset-TOPIC)
    (";r" . Domain-of-synset-REGION)
    (";u" . Domain-of-synset-USAGE)
    ("+" . Derivationally-related-form)))

#+test
(progn
  (ptr-symbols-from-strings 'n '("@" "~" "#m" "#p" "%s" "%p"))
  (noun-index-parse "orange n 5 6 @ ~ #m #p %s %p 5 3 07641918 04907947 12540089 14794019 09245156"))

#+test
(ptr-symbols-from-strings 'a '("&"))
;;; /// Have the following created by def-word-net-relations
;;;
(defun ptr-symbols-from-strings (pos sym-strings)
  (flet ((lookup (assoc)
           (loop for s in sym-strings
               collecting (or (cdr (assoc s assoc :test 'string=))
                            (break "unkown ptr sym ~a" s)
                            (intern s)))))
    (case pos
      (n (lookup '(("!" . Antonym)
                   ("@" . Hypernym)
                   ("@i" . Instance-Hypernym)
                   ("~" . Hyponym)
                   ("i" . Instance-Hyponym) ;; I think this is wrong and next is right but cannot hurt?
                   ("~i" . Instance-Hyponym)
                   ("#m" . Member-holonym)
                   ("#s" . Substance-holonym)
                   ("#p" . Part-holonym)
                   ("%m" . Member-meronym)
                   ("%s" . Substance-meronym)
                   ("%p" . Part-meronym)
                   ("=" . Attribute)
                   ("+" . Derivationally-related-form)
                   (";c" . Domain-of-synset-TOPIC)
                   ("-c" . Member-of-this-domain-TOPIC)
                   (";r" . Domain-of-synset-REGION)
                   ("-r" . Member-of-this-domain-REGION)
                   (";u" . Domain-of-synset-USAGE)
                   ("-u" . Member-of-this-domain-USAGE ))))
      (v (lookup '(("!" . Antonym)
                   ("@" . Hypernym)
                   ("~" . Hyponym)
                   ("*" . entailment)                 
                   (">" . cause)
                   ("^" . also-see)
                   ("$" . verb-group)
                   ("+" . Derivationally-related-form)
                   (";c" . Domain-of-synset-TOPIC)
                   (";r" . Domain-of-synset-REGION)
                   (";u" . Domain-of-synset-USAGE))))
      ((a s) ;; s is adjective satellite
       (lookup '(("!" . Antonym)
                 ("&" . similar-to)
                 ("<" . participle-of-verb)
                 ("\\" . pertainym)                 
                 ("=" . attribute)
                 ("^" . also-see)
                 ("+" . Derivationally-related-form)
                 (";c" . Domain-of-synset-TOPIC)
                 (";r" . Domain-of-synset-REGION)
                 (";u" . Domain-of-synset-USAGE))))
      (r (lookup '(("!" . Antonym)
                   ("\\" . derived-from-adjective)                 
                   (";c" . Domain-of-synset-TOPIC)
                   (";r" . Domain-of-synset-REGION)
                   (";u" . Domain-of-synset-USAGE)
                   ("+" . Derivationally-related-form))))
      (otherwise
       (break "ptr-symbols-from-strings> unknown POS ~a for syms ~a" sym-strings)))))


(defun wn-morph (w)
  (with-db (*ghx-reference*)
    (unless (word-net-lookup w)
      (let* ((w-stem (frz:porter-stem w))
             (w-stem-len (length w-stem)))
        (when (< w-stem-len (length w))
          (let (#+notyet(ending (subseq w w-stem-len)))
            (if (word-net-lookup w-stem)
                w-stem
              (bwhen (wnw (block kenny-morph
                            (progn ;; AC lacks create-index-cursor*
                              (db-index-generic-do 'word-net-verb 'lemma w-stem 
                                :callback (lambda (w-new)
                                            (return-from kenny-morph w-new)))
                              (db-index-generic-do 'word-net-adjective 'lemma w-stem 
                                :callback (lambda (w-new)
                                            (return-from kenny-morph w-new)))
                              (db-index-generic-do 'word-net-adverb 'lemma w-stem 
                                :callback (lambda (w-new)
                                            (return-from kenny-morph w-new)))
                              ; WN will put anything in nouns it seems, so trying to
                              ; get from dispens to dispense we would end up with dispensability
                              (db-index-generic-do 'word-net-noun 'lemma w-stem 
                                :callback (lambda (w-new)
                                            (return-from kenny-morph w-new))))))
                (lemma wnw)))))))))