(in-package :wordnet)



#+boom
(word-net-load)

(defun word-net-load ()
  (ghx-reference-open-ensure)
  
  (with-db (*ghx-reference*)
    (loop for part-of-speech in '(:adj :adv  :verb  :noun) 
        for pos-path = (merge-pathnames
                        (make-pathname :type 
                          (down$ (symbol-name part-of-speech)))
                        (merge-pathnames
                         (make-pathname :directory '(:relative "WordNet3.0"))
                         *cleaner-initial-resource-directory*))
        do (word-net-index-load part-of-speech
             (merge-pathnames (make-pathname :name "index") pos-path))
          (word-net-data-load (word-net-pos-to-synset-class-name part-of-speech)
            (merge-pathnames (make-pathname :name "data") pos-path)))
    (commit)
    (let ((n 0))
      (doclass* (e 'word-net-index-entry)
        (trc-every ((incf n) 5000) "resolving index synsets" n (synset-class e))
        (when (zerop (mod n 10000))
          (trc "committing")
          (commit))
        (setf (synsets e) (loop for synset-offset in (synsets e)
                              for synset = (db-get (synset-class e) 'offset synset-offset)
                              do (unless synset
                                   (describe e)
                                   (break "no synset for index ~a offset ~a class ~a right? ~a"
                                     e (cons (type-of synset-offset) synset-offset) (synset-class e)
                                     (db-get (synset-class e) 'offset synset)))
                              collecting (progn
                                           (assert (typep synset 'word-net-synset))
                                           synset))))
;     (trc "beginning to resolve related synsets")
      (doclass* (e 'word-net-synset)
        (trc-every ((incf n) 5000) "resolving related synsets" n e (related-synsets e))
        (when (zerop (mod n 10000))
          (trc "committing")
          (commit))
        (setf (related-synsets e)
          (loop for (relation synset-offset pos) in (related-synsets e)
              for synset = (db-get (word-net-pos-to-synset-class-name pos) 'offset synset-offset)
              ;; for n upfrom 0
              ;;do (break "rel ~a off ~a pos ~a" relation synset-offset pos)
              if synset
              collect (progn ;;(trc-every (n 100) "related sysnset ok" relation synset-offset pos)
                        (assert relation () "missing relation in synsets ~a in index ~a" (related-synsets e) e)
                        (make-instance relation :synset synset))
              else do (trc nil "ok not ok?" relation synset-offset pos)
                #+chill (break "cool?")))
        #+chill (break "one done")))
    (commit)))

#+test
(doclass* (e 'word-net-synset)
  (setf (related-synsets e)
    (loop for (relation synset-offset pos) in (related-synsets e)
        for synset = (db-get (word-net-pos-to-synset-class-name pos) 'offset synset-offset)
        if synset
        collect (make-instance relation :synset synset)
        else do (break "unable to convert fseek address to AC instance ~a" (cons pos synset-offset)))))

#+test
(progn
  (word-net-reset)
  (word-net-lookup "test"))
#+test
(let ((n 0))
  (doclass* (e 'word-net-index-entry)
    (trc-every ((incf n) 1000) "resolving index synsets" n (synset-class e))
    (setf (synsets e) (loop for synset in (synsets e)
                          collect (db-get (synset-class e) 'offset synset)))))

#+test
(db-get 'word-net-adverb-synset 'offset 250898)

(defun word-net-index-load (part-of-speech dictionary-path)
  (trc "loading index" part-of-speech dictionary-path)
  (with-open-file (file dictionary-path :direction :input)
    (commit :bulk-load :start)
    (loop with item = (make-string 1024)
        for (item-len reason) = (multiple-value-list (read-line-into item file nil :eof))
        for line-no upfrom 0
        
        for in-copyright = t then (and in-copyright (char= #\space (schar item 0)))
        until (eq reason :eof)
        unless in-copyright do
          (trc-every (line-no 5000) "new word" line-no (subseq item 0 (position #\space item)))
          
          (destructuring-bind (lemma  pos$  synset-cnt  p-cnt$ &rest more)
              (split-sequence #\space item :end item-len)
            (let ((p-cnt (parse-integer p-cnt$))
                  (pos (intern (string-upcase pos$))))
              (destructuring-bind (sense-cnt tagsense-cnt &rest synset-offsets)
                  (subseq more p-cnt)
                (declare (ignorable tagsense-cnt sense-cnt))
                #+shh (break "stroing ~a ~a ~a"
                  (nsubstitute #\space #\_ lemma) synset-offsets
                  (mapcar 'parse-integer (subseq synset-offsets 0 (parse-integer synset-cnt))))
                (make-instance (word-net-pos-to-index-class-name pos)
                  :lemma (nsubstitute #\space #\_ lemma)
                  :synsets (mapcar 'parse-integer (subseq synset-offsets 0 (parse-integer synset-cnt)))))))
        finally (commit :bulk-load :end)
          (trc "lines-processed" line-no))))

(defun word-net-data-load (synset-class dictionary-path)
  (trc  "loading data!!!!!!!!!!!!!!!" synset-class dictionary-path)
  (with-open-file (file dictionary-path :direction :input)
    (commit :bulk-load :start)
    (loop with item = (make-string 16384)
        for (item-len reason) = (multiple-value-list (read-line-into item file nil :eof))
        for line-no upfrom 0
                
        for in-copyright = t then (and in-copyright (char= #\space (schar item 0)))
        until (eq reason :eof)
        unless in-copyright do
          (trc-every (line-no 5000) "new data" line-no (subseq item 0 (position #\space item)))
          (destructuring-bind (synset-offset lex-filenum  ss-pos  w-cnt &rest more)
              (split-sequence #\space item :end (position #\| item :end item-len))
            (declare (ignore lex-filenum))
            ;;(trc "data" synset-offset lex-filenum  ss-pos  w-cnt (length more))
            
            (let* ((w-cnt (parse-integer w-cnt :radix 16))
                   
                   (words (loop repeat w-cnt
                                    for (w) on more by #'cddr
                                    collecting w)))
              (destructuring-bind (p-cnt &rest ptr-info)
                  (subseq more (* 2 w-cnt))
                ;;(trc "pcnt" p-cnt synset-offset (position #\| item :end item-len))
                (let* ((p-cnt (parse-integer p-cnt))
                       (related-synsets
                        (loop repeat p-cnt
                            for (pointer-symbol synset-offset pos nil #+oskool source-target) on ptr-info by #'cddddr
                              ;;do (trc "stuff" pointer-symbol synset-offset pos source-target)
                            unless (string= synset-offset "")
                            collecting (let ((rel-type (car (ptr-symbols-from-strings (intern (string-upcase ss-pos))
                                                              (list pointer-symbol)))))
                                         (unless rel-type
                                           (trace ptr-symbols-from-strings)
                                           (trc "BADITEM" (subseq item 0 item-len))
                                           (trc "BADMORE" more)
                                           (TRC "BADCNTS" w-cnt p-cnt)
                                           (trc "BADPTRINFO" ptr-info)
                                           (trc "BADPOS" (intern (string-upcase ss-pos)))
                                           (trc "BADPTR" (list pointer-symbol))
                                           (trc "BADRES" (ptr-symbols-from-strings (intern (string-upcase ss-pos))
                                                           (list pointer-symbol)))
                                           (assert rel-type () "no rel type for ~a" pointer-symbol))
                                         (list rel-type (parse-integer synset-offset)
                                           (intern (string-upcase pos))))))
                       (glossaries (bwhen (bar-pos (position #\| item :end item-len))
                                     (gloss-parse (string-trim " " (subseq item (1+ bar-pos) item-len))))))
                  ;; (trc "storing" (list lex-filenum word-lexids ptrs gloss))
                  (trc-every (line-no 5000) "new data" line-no (car glossaries))
                  (make-instance synset-class
                    :offset (parse-integer synset-offset)
                    :words words
                    :related-synsets related-synsets
                    :definition (car glossaries))))))
        finally (commit :bulk-load :end)
          (trc "lines-processed" line-no))))

(defun gloss-parse (gloss)
  (loop for seg in (split-sequence #\; gloss)
      collecting (string-trim " \"" seg)))




;; lemma  pos  synset_cnt  p_cnt  [ptr_symbol...]  sense_cnt  tagsense_cnt   synset_offset  [synset_offset...] 
;; orange n 5 6 @ ~ #m #p %s %p 5 3 07641918 04907947 12540089 14794019 09245156 

