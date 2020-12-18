(in-package :wordnet)

(defvar *synsub-paths* nil)
(defparameter *synsub-rel* nil)
(defparameter *synsub-nopath* nil)

#+test
(progn
  (word-net-reset)
  (ghx-reference-open-ensure)
  (with-db (*ghx-reference*)
    (loop for i in (word-net-lookup "plate")
          do (print i)
          (loop for ss in (synsets i)
                 ;for ss = (db-get 'word-net-synset 'offset synset-offset)
                 do
                   (print (list (lemma i) (length (related-synsets ss)) (definition ss)))
                   (loop for rs in (related-synsets ss)
                         do (trc "related" (type-of rs) (words (synset rs))))))
    #+chill
    (word-definitions "set")))

#+test
(progn
  (word-net-reset)
  (ghx-reference-open-ensure)
  (time (word-net-lookup "plate")))

#+test
(doclass (i 'word-net-noun)
  (print (synsets i))
  (break))

(defun downcase? (s)
  (every 'lower-case-p s))

(defun upcase? (s)
  (every 'upper-case-p s))

(defvar *missing* nil)

(defun link-words (w1 w2 &optional (wn-max-path 4))
  (wtrc (0 100 "link-words" w1 w2 wn-max-path)
    (count-it :link-words)
    (cond 
     ((string= w1 w2)
      (list (list :is-same-word w2)))
     
     ((let ((dw1 (down$ w1)))
        (unless (string= dw1 w1)
          (bwhen (link (link-words dw1 w2 wn-max-path))
            (cons (list :word-1 w1 :case-normalized-to dw1)
              link)))))
     
     ((let ((dw2 (down$ w2)))
        (unless (string= dw2 w2)
          (bwhen (link (link-words w1 dw2 wn-max-path))
            (cons (list :word-2 w2 :case-normalized-to dw2)
              link)))))
     
     ((bwhen (tw (singularify-ex w1))
        (bwhen (link (link-words tw w2 wn-max-path))
          (cons (list :word-1 w1 :in-singular-form tw)
            link))))
     
     ((bwhen (tw (singularify-ex w2))
        (bwhen (link (link-words w1 tw wn-max-path))
          (cons (list :word-2 w2 :in-singular-form tw)
            link))))
     
     ((bwhen (tw (singularify-ex w2))
        (bwhen (link (link-words w1 tw wn-max-path))
          (cons (list :word-2 w2 :in-singular-form tw)
            link))))
     
     ((loop for tw in (wn-unabbreviate (up$ w1))
          for link = (link-words tw w2 wn-max-path)
          thereis (when link
                    (cons (list :word-1 w1 :spelled-out tw)
                      link))))
     
     ((loop for tw in (wn-unabbreviate (up$ w2))
          for link = (link-words w1 tw wn-max-path)
          thereis (when link
                    (cons (list :word-2 w2 :spelled-out tw)
                      link))))
     
     ;
     ; the rest use wordnet which is only downcase
     ;
     (t (when (and (downcase? w1)(downcase? w2))
          (cond
           ((unless (word-net-lookup w1)
              (when (< (edit-distance w1 w2) 2)
                (list (list :w2 w2 :respells w1)))))
           
           ((unless (word-net-lookup w2)
              (when (< (edit-distance w1 w2) 2)
                (list (list* :w1 w1 :respells w2)))))
           
           ((bwhen (tw (wn-morph w1))
              (bwhen (link (link-words tw w2 wn-max-path))
                (break "bingo w1 morphed!!! ~a ~a" w1 tw)
                (cons (list :word-1 w1 :morphed-to tw)
                  link))))
           
           ((bwhen (tw (wn-morph w2))
              (bwhen (link (link-words w1 tw wn-max-path))
                (break "bingo w2 morphed!!! ~a ~a" w2 tw)
                (cons (list :word-2 w2 :morphed-to tw)
                  link))))
           
           (t 
            (bif (hopeless (remove-if (lambda (w)
                                        (word-net-lookup (down$ w)))
                             (list w1 w2)))
              (progn
                (loop for w in hopeless
                    do (pushnew  w *missing* :test 'string-equal))
                (values nil (list (list* :hopeless-words hopeless))))
              (bwhen (paths (wn-connect-a-dot w1 w2 wn-max-path))
                (push (list w1 *synsub-rel* paths w2) *synsub-paths*)
                (list (list* :word-net-path paths)))))))))))

(defun wn-porter-stem (w)
  (let ((s (frz:porter-stem w)))
    (cond
     ((string= s w) nil)
     ((word-net-lookup s) s))))

(defun wn-unabbreviate (w)
  (unless (word-net-lookup (down$ w))
    (unabbreviate w)))

(defun porter-stem-only (w)
  (let ((s (frz:porter-stem w)))
    (unless (string= s w)
      (when (word-net-lookup s)
        s))))

#+test
(loop for w in '("expanding" "expand" "expandable" "incisional" "incision" "expanse")
    for s = (wn-porter-stem w)
    do (trc "ws" w s (word-net-lookup w)))

#+test 
(wn-porter-stem "secure")
#+test
(porter-stem-only "expandable")
#+test
(progn
  (word-net-reset)
  (print (link-words "expanding" "expandable")))

#+test
(progn
  (word-net-reset)
  (print (word-net-lookup "fasten")))

(defun test-connect ()
  (test-setup)
  (let ((w1 "knot")
        (w2 "secure"))
    (trc "Word 1:" w1 :ghx-defs (ghx-definitions w1))
    (loop for d in (word-net-lookup w1)
          do (trc "  ss1:" d))
    (trc "Word 2:" w2 :ghx-defs (ghx-definitions w2))
    (loop for d in (word-net-lookup w2)
        do (trc "  ss2:" d))
    (print (wn-connect-a-dot w1 w2 2))))
      
(defstruct (ss-search-node (:conc-name ss-node-))
  cost
  value ;; sense-index-entry, synset, or related-synset
  reached-from)

(defmethod current-synset ((self word-net-synset)) self)
(defmethod current-synset ((self word-net-relation))(synset self))
(defmethod current-synset ((self ss-search-node))
  (current-synset (ss-node-value self)))

(defmethod relation-cost (self)
  (declare (ignore self))
  5) ;; default a little weighty to leave room for synonymic lower cost stuff

(defmethod relation-cost ((self DOMAIN-OF-SYNSET-TOPIC)) 10) ;; because we are getting vague
(defmethod relation-cost ((self DERIVATIONALLY-RELATED-FORM)) 3)

(defmethod node-cost :around (self) (when self (count-it :node-cost)) (call-next-method))
(defmethod node-cost ((self word-net-synset)) 0)
(defmethod node-cost ((self ss-search-node)) (ss-node-cost self))

(defvar *wn-connect-a-dot*)

(defun wn-connect-a-dot (w1 w2 &optional (max-depth 4))

  (when (and (boundp '*wn-connect-a-dot*)
          *wn-connect-a-dot*)
    (let ((h (gethash (list w1 w2 max-depth) *wn-connect-a-dot* :nope)))
      (unless (eq h :nope)
        (return-from wn-connect-a-dot h))))

  (let ((wn1 (word-net-lookup w1))
        (wn2 (word-net-lookup w2)))
    (when (and wn1 wn2)
      ;(loop for s in wn1 do (print `(wn1 ,s)))
      ;(loop for s in wn2 do (print `(wn2 ,s)))
      (loop initially (trc "wn-connect-a-dot senses:" (length wn1)(length wn2) max-depth)
          with candidate-paths
          for sense-1 in wn1
          do (loop for sense-2 in wn2
                 do (bwhen (paths (find-paths-between-senses sense-1 sense-2 max-depth 1))
                      (setf candidate-paths (nconc candidate-paths paths))))
          finally
            (when (and (boundp '*wn-connect-a-dot*)
                    *wn-connect-a-dot*)
              (setf (gethash (list w1 w2 max-depth) *wn-connect-a-dot*) candidate-paths))
            (return candidate-paths)))))

(defun find-paths-between-senses (sense-1 sense-2
                                  &optional (depth-remaining 0) (extra-innings 5)
                                  &aux (ss-visited (make-hash-table :test 'eq)))
  (if (eq sense-1 sense-2)
      (list :common-sense sense-1 sense-2))
  (let ((search-list (loop for ss in (synsets sense-1)
                         collecting (make-ss-search-node
                                     :cost 1
                                     :value ss
                                     :reached-from sense-1)))
        (ss-goals (synsets sense-2))
        paths-found)
    (trc "fpath> goals" (LENGTH ss-goals))
    (tagbody seek-shared
      (unless search-list (return-from find-paths-between-senses)) ; unlikely
      ; (trcx seeklength (length search-list) depth-remaining)
      (count-it :seek-shared)
      (sum-it (length search-list) :seek-shared-lengths)

      (loop for search in search-list
          when (find (current-synset search) ss-goals :test 'eq)
          collect (make-ss-search-node
                   :cost (node-cost search)
                   :value sense-2
                   :reached-from search) into ss-shared
          finally (when ss-shared
                    (when paths-found
                      (trc "bingo extra innings ~a costs ~a" (cons sense-1 sense-2)
                        (mapcar 'node-cost paths-found)))
                    (setf paths-found (merge 'list paths-found 
                                        (sort ss-shared '< :key 'ss-node-cost)
                                        '< :key 'node-cost))
                    (setf depth-remaining extra-innings))) ;; could increase or decrease (zero forces exit)
      
      (when (minusp (decf depth-remaining))
        (return-from find-paths-between-senses paths-found))

      (setf search-list
        (let ((x (loop for search in search-list
                     unless (> (node-cost search) 35)
                     nconcing (ss-search-continuations search ss-visited))))
          #+nah (setf x (sort x '< :key 'node-cost))
          x))
      (trc nil "go seek shared ~a" (length search-list))
      (go seek-shared))))

(defun ss-search-continuations (search-node visited)
  (let ((cs (loop for rss in (related-synsets (current-synset search-node))
                unless (or (gethash (synset rss) visited)
                         (typep rss 'antonym))
                do (setf (gethash (synset rss) visited) t)
                  (count-it :new-search-node)
                and collect (path-penalized
                             (make-ss-search-node
                              :cost (+ (node-cost search-node)
                                      (relation-cost rss))
                              :value rss
                              :reached-from search-node)))))
    ;;(sort CS '< :key 'node-cost)
    CS))

(defun path-penalized (p)
  (count-it :path-penalized)
  (prog1 p
    ;(trcx pp-begin (type-of p) (ss-node-cost p)(type-of (ss-node-reached-from p)))
    (loop with state = 'init
        and penalty = 0
        and reversals = 0
        for step = p then (ss-node-reached-from step)
        while (typep step 'ss-search-node) do
          ;(print `(stepp ,(type-of step) :value ,(type-of step)))
          (typecase (ss-node-value step)
            ((or hypernym part-holonym member-holonym substance-holonym ;; semantically less specific
               Domain-of-synset-TOPIC Domain-of-synset-REGION) ;; these are lexically less specific
             ;(trc "bingo state ~a" state)
             (case state
               (going-up (incf penalty 2)) ;; subsequent ups are worser than just one?
               (going-down 
                ;(break "bingo 10")
                (incf reversals)
                (incf penalty (* reversals 10))) ;; reversals are nuts
               )
             (setf state 'going-up))
            ((or hyponym part-meronym member-meronym substance-meronym ;; semantically more specific
               Member-of-this-domain-TOPIC Member-of-this-domain-REGION) ;; lexically more specific
             ;(trc "bingo 2 state ~a" state)
             (case state
               (going-down (incf penalty 2)) ;; subsequent downs are worser than just one?
               (going-up
                ;(break "bingo 10 2")
                (incf reversals)
                (incf penalty (* reversals 10))) ;; reversals are nuts
               )
             (setf state 'going-down))
            (otherwise
             (trc nil "hunh?" (type-of (ss-node-value step)))))
        finally
          ;;(print `(exit coost ,(ss-node-cost p)))
          (incf (ss-node-cost p) penalty)
          #+chill (when (plusp penalty)
            (break "path ~a penalty ~a" p penalty (ss-node-cost p)))
          )))
               
               
      
#+test
(frz:porter-stem "expanding")

(defun link-words-do (w1 w2)
  (bif (link (link-words w1 w2))
    (cons (list :comparing-word-1 w1 :with-word-2 w2) link)
    (list w1 :unrelated-to w2)))

(defun test-link-words ()
  (loop for (w1 w2) in  '(("cat" "cat")
                          ("media" "medium")
                          ("medium" "media" )
                          ("medium" "Media" )
                          ("meDium" "Media" )
                          ("vlv" "vALve"))
      do (print (link-words-do w1 w2))))

(defun word-leaves (w &key (dos 2) poses (types '(hypernym)) (not-types '(antonym)))
  (unless (listp types)(setf types (list types)))
  (unless (listp not-types)(setf not-types (list not-types)))
  (loop with visited = (make-hash-table :test 'eq)
      and leaves = (make-hash-table :test 'eq)
      for sense in (word-net-lookup w)
      do (loop 
           for synset in (synsets sense)
           do (find-leaves (list (type-of sense)) 'root synset poses types not-types (1- dos) visited leaves))))

(defun find-leaves (types-followed offset-type synset poses types not-types dos visited leaves)
  (declare (ignorable types-followed types not-types))
  
  (trc nil "flv> entry ~a" (list offset-type synset dos (gethash synset visited)))
  (unless (or (minusp dos)(gethash synset visited))
    (setf (gethash synset visited) t)
    (if (zerop dos)
        (When (or (null poses)(break "pos fnyi")(find (type-of synset) poses))
          (unless (gethash synset leaves)
            (setf (gethash synset leaves)
              (list offset-type (words synset) (definition synset)))
            (trc "w:" (offset synset) (type-of synset) offset-type (car (words synset)) (left$ (definition synset) 40))
            #+shh (loop for tf in types-followed
                      do (trc "  tf:" tf))))
      (loop for rel-synset in (related-synsets synset)
          for rel-type = (type-of rel-synset)
          when (and ;; way too restrictive (eq next-pos pos)
                (or (null types)(find rel-type types))
                (not (find rel-type not-types)))
          do (find-leaves (cons (cons rel-type (car (words synset))) types-followed)
                rel-type (synset rel-synset) poses types not-types (1- dos) visited leaves)))))

(defun wni-pos (wni)
  (etypecase wni
    (word-net-noun 'n)
    (word-net-verb 'v)
    (word-net-adjective 'a)
    (word-net-adverb 'r)))

(defun word-definitions (word$)
  (ghx-reference-open-ensure)
  (with-db (*ghx-reference*)
    (loop for wni in (word-net-lookup word$)
          collecting (cons (wni-pos wni)
                       (loop for synset in (synsets wni)
                           collecting (definition synset))))))

#+test
(word-definitions "plate")

#+test
(word-net-lookup "plate")

(defstruct (wnlink (:type list))
   lemma type synset)

(defun map-value* (map key)
  (count-it :mapvalue)
  (map-value map key))

(defun word-path-try (synset to-pos to-offset 
                       following-types not-following-types
                       depth-remaining
                       &optional (visited (make-hash-table :test 'eq)))

  (when (gethash synset visited)
    (return-from word-path-try))
  (setf (gethash synset visited) t)
  (count-it :word-path-try)
  (progn ;; wtrc (0 100 "wptx:" depth-remaining (car (words synset)) (offset synset) (type-of synset))
    (when (and (typep synset to-pos)
            (= (offset synset) to-offset))
      (trc nil "try found target!!!" to-pos to-offset synset)
      ;;(break "wptx-bingo ~a ~a ~a" try-offset-type try-pos try-offset)
      #+chill (when (and (eq try-pos 'n)
              (eql try-offset 5964779))
        (break "medicine"))
      (return-from word-path-try (list synset)))

    (when (plusp depth-remaining)
      (loop for rel-synset in (related-synsets synset)
          for rel-type = (type-of rel-synset)
          thereis (bwhen (path (and
                                (or (null following-types) (find rel-type following-types))
                                (not (find rel-type not-following-types))
                                (word-path-try (synset rel-synset)
                                  to-pos to-offset
                                  following-types not-following-types
                                  (1- depth-remaining) visited)))
                    (cons rel-synset path))))))


(defun word-type-path-p (word-type word category)
  (break "oldskool" word-type word category)
  #+nahhh
  (labels ((try-offset (pos offset &aux (map (wn-pos-map pos)))
             (loop for (nil word-lexids ptrs nil) in (map-value map offset)
                 do (if (find category word-lexids :key 'car :test 'string-equal)
                        (return-from word-type-path-p t)
                      (loop for (type offset pos nil) in ptrs
                          when (eq type word-type)
                          do (try-offset pos offset))))))
    (or (string-equal word category)
      (loop for (pos nil offsets) in (word-net-lookup word)
          do (loop for offset in offsets
                 do (try-offset pos offset))))))

