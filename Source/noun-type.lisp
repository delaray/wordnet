(in-package :wordnet)

(defparameter *unspsc-obsolete*
  '(42181909 42241810 13102009 13102015 13102023 13102031 41112218
    11101703 11101707 11101715 30201713 23171518 23171519
    23171520 23171521 23171528 23171540 23171703 23171508 23171530
    23171500 23171605 23171606 23171607 23171608 23171611
    23171617    23171622    23171623    23172001    23172003    23172005    23153302    23153306
    23153307    23153309    23153311    23172000    23153300    23171800    23161504    23161508
    23161509    23161513    23161515    23171600    23171901
    23171902 23171903 23171904 23171905 23171906 23171909
    23171910 23171900 23171706 23171700    39101606    39101610    39101701    39101700
    39111516    39121608    39121712    39121200    42295507))

#+notused
(defun noun-type-swap ()
  (noun-type-swap-with-policy :moderate))

#+TEST
(WITH-DB (*resource-raw*)
  (list
   (map-value (find-tag-map "TYPE") "CIRCUMCISION")
   (map-value (find-tag-map "NOUN") "CLAMP")))

#+notused
(defun noun-type-swap-with-policy (policy)
  (let ((noun (make-hash-table :test 'equal))
        (type (make-hash-table :test 'equal))
        (n-raw (find-tag-map "NOUN" :db *resource-raw*))
        (t-raw (find-tag-map "TYPE" :db *resource-raw*))
        (n-fix (find-tag-map "NOUN" :db *tag-repair*))
        (t-fix (find-tag-map "TYPE" :db *tag-repair*))
        (total 0))
    (assert (and n-raw t-raw))
    (assert (and n-fix t-fix))
    (map-map* n-raw
      (lambda (w ct)
        (incf (gethash (or (get-spelling-fix n-fix w)
                         (get-spelling-fix t-fix w)
                         w) noun 0) ct)))
    (map-map* t-raw
      (lambda (w ct)
        (incf (gethash (or (get-spelling-fix t-fix w)
                         (get-spelling-fix n-fix w)
                         w) type 0) ct)))
    (flet ((move (msg w dest wct lct delta)
             (unless (ecase policy
                       (:aggressive nil)
                       (:eager (> lct (/ wct 5)))
                       (:moderate (or (find msg '(:wrong))
                                    (> lct (/ wct 10))))
                       (:conservative 
                        (or (find msg '(:wrong :both))
                          (> lct (/ wct 15)))))
               (incf total)
               (ecase dest
                 (type (push `(:move type ,wct ,lct)
                         (map-value n-fix w)))
                 (noun (push `(:move noun ,wct ,lct)
                         (map-value t-fix w))))
               (trc-every (total 20) ">>" msg w dest wct lct delta)
               )))
      (loop for n being the hash-keys of noun
          using (hash-value nct)
            ;;   repeat 100
          for tct = (gethash n type)
          for delta = (when tct (abs (- tct nct)))
            ;; for max = (when tct (max nct tct))
          when tct ;; (and tct (> max 2))
          if (> tct nct)
          do (let ((msg (if (master-type-p n)
                            (if (master-noun-p n)
                                :both :type)
                          (if (master-noun-p n)
                              :wrong :neither))))
               (move msg n 'type tct nct delta))
          else
          do (let ((msg (if (master-type-p n)
                            (if (master-noun-p n)
                                :both :wrong)
                          (if (master-noun-p n)
                              :noun :neither))))
               (move msg n 'noun nct tct delta))
          finally (print `(total ,total))))))

(defclass* noun-type-master (:print nil)
  (noun-type :index :any :documentation "eg, SCREW|BONE for n=screw, t=bone")
  ;;
  ;; rest just copied from tab-delim Master Noun/Type,
  ;; file name MASTER_NT_ONLY_18980_060507.xls,
  ;; provided by GHX.
  ;;
  unique-id
  (taxonomy-node :index :any)
  commodity ;; name is from header, but looks like "description"
  brand-name
  vendor)

#+lispsucks
(with-open-file (source (make-cleaner-resources-pathname "nt-master-dos.txt") :direction :input)
    (let ((item (read-line source nil :eof)))
      (print (length item))
      (inspect (subseq item 0 1000))))

#+whatever
(master-noun-type-load)

#+zap
(progn
  (ghx-reference-open-ensure)
  (zap-ac-class 'noun-type-master)
  (commit))

#+test
(doclass (m 'noun-type-master)
  (when (or (vendor m) #+bzzt(brand-name m))
    (trc "ntm" (noun-type m) (brand-name m)(vendor m))
    (describe m)
    (break)))

#+test
(nt-masters-get "SCREW|BONE")

(defun nt-masters-get (nt-key)
  (with-db (*ghx-reference*)
    (db-get-all 'noun-type-master 'noun-type nt-key)))

#+test
(master-noun-type-load)

(defun master-noun-type-load ()
  (test-setup)
  (ghx-reference-open-ensure)
  (let ((*log* nil)
        (dir-path (make-cleaner-resources-pathname "nt-master-dos.txt"))
        (noun-map (recreate-tag-map "MASTER-NOUN"
                    :type 'ac-map-range))
        (type-map (recreate-tag-map "MASTER-TYPE"
                    :type 'ac-map-range))
        (tot 0))
    (labels ((store-map (map value)
               (let ((ct (or (map-value map value) 0)))
                 (setf (map-value map value) (1+ ct))))
             
             (resourcer (item item-len header-info item-fields)
               (declare (ignorable item item-len header-info item-fields))
               (incf tot)
               (let ((noun .noun)
                     (type .type))
                 (make-instance 'noun-type-master
                   :noun-type (make-noun-type-key noun type)
                   :commodity (itmf commodity)
                   :unique-id (itmf uniqueid)
                   :taxonomy-node (itmf taxonomynode)
                   :brand-name (when (plusp (length (itmf |BRAND NAME|))) (itmf |BRAND NAME|))
                   :vendor (when (plusp (length (itmf vendor))) (itmf vendor)))
                 (when nil ;; (zerop (mod tot 1000))
                   (trc "noun-type-master" noun type (itmf commodity)(itmf taxonomynode)(itmf |BRAND NAME|)(itmf vendor)))
                 (store-map noun-map noun)
                 (store-map type-map type))))
      
      (map-delimited dir-path nil	
        :map-fn #'resourcer
        :partial-handling :ignore
        :long-handling    :process
        :short-handling   :process
        :end nil
        :parse-known-tags? nil)
      (commit))))



;; (:SPOGI :POSGI :OSPGI :GSPOI :GPOSI :GOSPI)

#+waitfor2.2
(defun master-noun-type-load-ag ()
  (test-setup)
  
  (create-triple-store "noun-type-master"
    :with-indices '(:SPOGI :POSGI :OSPGI)
    :directory *cleaner-data-directory*
    :if-exists :supersede)

  (let ((*synchronize-automatically* nil)
        (*log* nil)
        (dir-path (make-cleaner-resources-pathname "nt-master-dos.txt"))
        (tot 0)
        (ibrand (intern-resource "http://www.ghx.com#brand"))
        (ivendor (intern-resource "http://www.ghx.com#vendor"))
        (itranslates-to-unspsc (intern-resource "http://www.ghx.com#translates-to-unspsc"))
        (ihas-commodity (intern-resource "http://www.ghx.com#has-commodity"))
        (known-uniqueids (make-hash-table :test 'equal)))

      (labels ((resourcer (item item-len header-info item-fields)
                 (declare (ignorable item item-len header-info item-fields))
                 (incf tot)
                 ;(break "~a ~a ~a" (car .noun)(car .type)(itmf taxonomynode))
                 
                 ;(trc-every (tot 1000) "xx" tot (car .noun)(car .type)(itmf taxonomynode))
                 
                 (let ((inoun (intern-literal (car .noun)))
                       (itype (intern-literal (car .type)))
                       (itaxo (intern-literal (itmf taxonomynode))))
                       
                   (assert (itmf uniqueid))
                   (let* ((iuniqueID (intern-literal (itmf uniqueid)))
                          (tr (add-triple inoun itype iuniqueID))
                          (ntr (value->upi tr :triple-id))
                          )
                     
                     (add-triple-o-literal ntr ibrand (itmf |BRAND NAME|))
                     (add-triple-o-literal ntr ivendor (itmf vendor))
                     ;;
                     ;; now normalize UNSPSC info
                     ;;
                     (unless (gethash (itmf uniqueid) known-uniqueids)
                       (setf (gethash (itmf uniqueid) known-uniqueids) t)
                       (add-triple iuniqueID itranslates-to-unspsc itaxo)
                       (add-triple-o-literal itaxo ihas-commodity (itmf commodity)))))))
        (map-delimited dir-path nil
          :map-fn #'resourcer
          :partial-handling :ignore
          :long-handling    :process
          :short-handling   :process
          :end nil
          :parse-known-tags? nil)
        
        (unless *synchronize-automatically*
          (index-new-triples))

        (trc "masters processed:" tot :to-triples (triple-count) :indexed (triple-store-indices)
          ))))



#+test
(time (master-noun-type-load-ag))
; cpu time (non-gc) 2,048 msec user, 282 msec system
; cpu time (gc)     62 msec user, 0 msec system
; cpu time (total)  2,110 msec user, 282 msec system
; real time  2,438 msec
; space allocation:
;  300,093 cons cells, 10,515,984 other bytes, 2,400,120 static bytes

;;;(defun master-noun? (s)
;;;  "Is string S a known noun in the GHX master noun-type sheet?"
;;;  (get-triples-list :s (intern-literal s) :limit 1))
;;;
;;;(defun master-type? (s)
;;;  "Is string S a known type in the GHX master noun-type sheet?"
;;;  (get-triples-list :p (intern-literal s) :limit 1))
;;;
;;;(defun ag-unspsc-commodity (taxo)
;;;  (assert taxo)
;;;  (break "fnyi"))
;;;
;;;(defun add-triple-o-literal (s p o)
;;;  (when o
;;;    (add-triple s p (intern-literal o))))

#+test
(indexing-needed-p)

#+test
(index-new-triples)


; cpu time (non-gc) 2,219 msec user, 468 msec system
; cpu time (gc)     31 msec user, 0 msec system
; cpu time (total)  2,250 msec user, 468 msec system
; real time  2,750 msec
; space allocation:
;  300,114 cons cells, 13,441,920 other bytes, 2,400,120 static bytes


(defun ac-nt-build ()
  (time (ghx-reference-create))
  (time (master-noun-type-load)))

#+test
(ac-nt-build)
; cpu time (non-gc) 2,938 msec user, 16 msec system
; cpu time (gc)     78 msec user, 0 msec system
; cpu time (total)  3,016 msec user, 16 msec system
; real time  3,063 msec
; space allocation:
;  1,056,819 cons cells, 13,187,664 other bytes, 0 static bytes



;; null run against NT:
; cpu time (non-gc) 546 msec user, 16 msec system
; cpu time (gc)     0 msec user, 0 msec system
; cpu time (total)  546 msec user, 16 msec system
; real time  594 msec
; space allocation:
;  295,061 cons cells, 4,872,296 other bytes, 0 static bytes

#+hunh
(dump-text (make-cleaner-resources-pathname "master_nt_only_18980_060507.CSV"))

#+boom
(master-noun-type-load)

#+test
(ghx-reference-open-ensure)

#+test
(list (master-noun-p "SCREW")(master-type-p "BONE"))

(defun master-noun-p (w)
  (with-db (*ghx-reference*)
    (when w
      (map-value (find-map "MASTER-NOUN") w))))

#+test
(with-db (*ghx-reference*)
  (map-count (find-map "MASTER-TYPE")))

(defun master-type-p (w)
  (with-db (*ghx-reference*)
    (when w
      (map-value (find-map "MASTER-TYPE") w))))

(defun get-word (s start &optional (end (length s)))
  (let ((w (subseq s start end)))
    (when (plusp (length w))
      w)))

(defun make-noun-type-key (noun type)
  (assert (and (atom noun)(atom type)))
  (concatenate 'string noun "|" type))

(defun get-nt-noun (self)
  (etypecase self
    (string (get-word self 0 (search "|" self)))))

(defun get-nt-type (self)
  (etypecase self
    (string (get-word self (+ 1 (search "|" self))))))

#+chill
(defun add-nt-describers (&aux (x 0))
  (doclass (nt 'noun-type)
    (when (get-nt-noun nt)
      (trc "start nt" (noun-type nt)(occurrences nt))
      (let (ds (rs 0))
        (block study-nt
          (db-index-do 'resource 'noun-type nt
            :callback (lambda (r)
                        (trc-every ((incf x) 5000) "r prog" x (noun-type r))
                        (when (> (Incf rs) 5000) (return-from study-nt))
                        (let ((words (delete-duplicates 
                                      (nconc (split-tag-phrase (itdesc r))
                                        (split-tag-phrase (second (find 'other (tags r) :key 'car))))
                                      :test 'string-equal)))
                          ;(unless *go-go* (print words))
                          (loop for ww in words
                              for w = (string-trim ";,." ww)
                              do (unless (find-if (lambda (c) (or (digit-char-p c) (find c "&*/."))) w)
                                   (bif (e (assoc w ds :test 'string-equal))
                                     (incf (cdr e))
                                     (push (cons w 1) ds))))))))
        (setf (describers nt) (sort (delete-if (lambda (e)
                                                 (< (cdr e) (/ (occurrences nt) 25))) ds)
                                '> :key 'cdr))
        (trc "done nt" (describers nt))
        (unless *go-go* (break)))))
  (commit))

(defun noun-preferred (s1 s2 &optional (ignoring " -/"))
  (loop ;with s1 = (term-word t1) and s2 = (term-word t2)
      with l1 = (length s1) and n1 = 0 and ignored-1 = 0
      with l2 = (length s2) and n2 = 0 and ignored-2 = 0
      do (cond
          ((and (< n1 l1)(find (schar s1 n1) ignoring)) (incf n1)(incf ignored-1))
          ((and (< n2 l2)(find (schar s2 n2) ignoring)) (incf n2)(incf ignored-2))
          ((and (< n1 l1)(< n2 l2)) (if (char-equal (schar s1 n1)(schar s2 n2))
                                        (progn (incf n1)(incf n2))
                                      (return-from noun-preferred nil)))
          (t (return (when (and (= n1 l1)(= n2 l2))
                       (if (master-noun-p s1)
                           (if (master-noun-p s2)
                               (if (<= ignored-1 ignored-2)
                                   s1 s2)
                             s1)
                         (if (master-noun-p s2)
                             s2
                           (if (<= ignored-1 ignored-2)
                               s1 s2)))))))))

(defun type-preferred (s1 s2 &optional (ignoring " -/"))
  (loop ;;with s1 = (term-word t1) and s2 = (term-word t2)
      with l1 = (length s1) and n1 = 0 and ignored-1 = 0
      with l2 = (length s2) and n2 = 0 and ignored-2 = 0
      do (cond
          ((and (< n1 l1)(find (schar s1 n1) ignoring)) (incf n1)(incf ignored-1))
          ((and (< n2 l2)(find (schar s2 n2) ignoring)) (incf n2)(incf ignored-2))
          ((and (< n1 l1)(< n2 l2)) (if (char-equal (schar s1 n1)(schar s2 n2))
                                        (progn (incf n1)(incf n2))
                                      (return-from type-preferred nil)))
          (t (return (when (and (= n1 l1)(= n2 l2))
                       (if (master-type-p s1)
                           (if (master-type-p s2)
                               (if (<= ignored-1 ignored-2)
                                   s1 s2)
                             s1)
                         (if (master-type-p s2)
                             s2
                           (if (<= ignored-1 ignored-2)
                               s1 s2)))))))))