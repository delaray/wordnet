(in-package :wordnet)

(defparameter *word-net-cache* nil)

#+test
(hash-table-count *word-net-cache*)

#+test
(loop for k being the hash-keys of *word-net-cache*
      summing (length k))

(defun word-net-reset ()
  (setf *word-net-cache* (make-hash-table :test 'equal :size 500000
                           :rehash-threshold .7 :rehash-size 1.5)))

(defun word-net-lookup (w &optional (type 'word-net-index-entry))
  "Return all definitions as any part-of-speech with second value of word searched, why I do not know (vestigial I think)"
  (assert *ghx-reference* () "Please open GHX-Reference DB before calling WORD-NET-LOOKUP")
  (assert *word-net-cache* () "Please call WORD-NET-RESET before calling WORD-NET-LOOKUP")
  ;(assert (every 'lower-case-p w) () "Word Net is always indexed in lowercase, word supplied ~S not lower-case-p." w)
  (multiple-value-bind (value presentp)
      (gethash (cons type w) *word-net-cache*)
    (if presentp
        value
      (setf (gethash (cons type w) *word-net-cache*)
        (with-db (*ghx-reference*)
          (count-it :word-net-lookup)
          (db-get-all* type 'lemma w))))))

(defun word-net-lookup? (w &optional (types '(word-net-noun word-net-verb word-net-adverb word-net-adjective)))
  (assert *ghx-reference* () "Please open GHX-Reference DB before calling WORD-NET-LOOKUP")
  (assert *word-net-cache* () "Please call WORD-NET-RESET before calling WORD-NET-LOOKUP")
  ;(assert (every 'lower-case-p w) () "Word Net is always indexed in lowercase, word supplied ~S not lower-case-p." w)
  (with-db (*ghx-reference*)
    (dolist (type types)
      (when (db-get type 'lemma w :oid t)
        (return t) ))))
        
#+test 
(progn
  (word-net-reset)
  (ghx-reference-open-ensure)
  (multiple-value-list
    (word-net-lookup "plate")))

#+test
(noun-p "piston")

(defun noun-p (word$)
  (serves-as-part-of-speech-p word$ 'word-net-noun))

(defun verb-p (word$)
  (serves-as-part-of-speech-p word$ 'word-net-verb))

(defun adjective-p (word$)
  (serves-as-part-of-speech-p word$ 'word-net-adjective))

(defun adverb-p (word$)
  (serves-as-part-of-speech-p word$ 'word-net-adverb))

(defun serves-as-part-of-speech-p (word$ part-of-speech)
  (find part-of-speech (word-net-lookup word$) :key 'type-of))

(defun singularify-ex (w &optional (validator (lambda (w)
                                             (when (word-net-lookup w) w))) &aux (l (length w)))
  "downcase only, please"
  ;
  ; this is pretty shaky reasoning
  ; expect trouble and need for improvement
  ;
  
  (flet ((lookup (w) (funcall validator w)))
    (cond
     ((and (char= #\s (schar w (1- l)))
        (lookup (left$ w (1- l)))))
     ((and (char= #\s (schar w (1- l)))
        (char= #\e (schar w (- l 2)))
        (lookup (left$ w (- l 2)))))
     ((and (char= #\a (schar w (1- l)))
        (char= #\i (schar w (- l 2)))
        (lookup (conc$ (left$ w (1- l)) "um"))))
     ((and (char= #\s (schar w (1- l)))
        (char= #\e (schar w (- l 2)))
        (char= #\i (schar w (- l 3)))
        (lookup (conc$ (left$ w (- l 3)) "y")))))))

#+test
(word-net-lookup "blade")

(defun singularify (w &optional (validator (lambda (w)
                                             (when (word-net-lookup w) w)) validator-supplied-p) &aux (l (length w)))
  "downcase only, please"
  ;
  ; this is pretty shaky reasoning
  ; expect trouble and need for improvement
  ;
  
  (flet ((lookup (w) (funcall validator w)))
    (if (lookup w)
        (values w nil)
      (bwhen (s (cond
                 ((and (char= #\s (schar w (1- l)))
                    (lookup (left$ w (1- l)))))
                 ((and (char= #\s (schar w (1- l)))
                    (char= #\e (schar w (- l 2)))
                    (lookup (left$ w (- l 2)))))
                 ((and (char= #\a (schar w (1- l)))
                    (char= #\i (schar w (- l 2)))
                    (lookup (conc$ (left$ w (1- l)) "um"))))
                 ((and (char= #\s (schar w (1- l)))
                    (char= #\e (schar w (- l 2)))
                    (char= #\i (schar w (- l 3)))
                    (lookup (conc$ (left$ w (- l 3)) "y"))))
                 ((char= #\s (schar w (1- l)))
                  (bif (a (unless validator-supplied-p
                            (singularify w (lambda (ws)
                                             (unless (char= #\s (schar w (1- (length ws))))
                                               (when (abbreviate (up$ ws))
                                                 ws))))))
                    a
                    #+shhh (format t "~&Can this be made singular? ~a" w)))))
        (values s t)))))

(defun pertainym-p (word domain)
  "Is <word> of or pertaining to <domain>?"
;;; A relational adjective. Adjectives that are pertainyms are 
;;; usually defined by such phrases as "of or pertaining to" and 
;;; do not have antonyms. A pertainym can point to a noun or another pertainym.
  (word-type-path-p 'pertainym word domain))

(defun hypernym-p (word category)
  "Is <word> a kind-of <category>? Inverse of hyponym-p."
  (word-type-path-p 'hypernym word category))

(defun hyponym-p (category word)
  "Is <word> a kind-of <category>? Inverse of hypernym-p."
  (word-type-path-p 'hyponym category word))

#+test
(hyponym-p "bone" "femur")

(defun meronym-p (part whole)
  "Is <part> a constituent part of, the substance of, or a member of <whole>?"
  (word-type-path-p 'meronym part whole))

(defun holonym-p (part whole)
  "Is <part> a constituent part of, the substance of, or a member of <whole>?"
  (word-type-path-p 'holonym whole part))

(defun color-p (w)
  (hypernym-p (string-downcase w) "color"))

(defun show-joint ()
  (loop for d in (noun-p "joint")
        do (print d)))

#+test
(inspect (noun-p "joint"))

#+test
(list (color-p "red")(color-p "bull"))

(defun flavor-p (w)
  (hypernym-p (string-downcase w) "flavor"))

(defun fragrance-p (w)
  (break "redo fragrance-p" w)
  #+chya
  (word-path-try w "plant_material"
    :max-dos 6
    :to-pos 'n))

(defun gender-p (w)
  (hypernym-p (string-downcase w) "gender"))

(defun wdump ()
  (print (color-p "red")))
