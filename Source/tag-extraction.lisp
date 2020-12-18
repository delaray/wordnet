(in-package :wordnet)

;;; --- the beef: process one file ---------------------------------------------------
;;;
(defvar *raw-item* nil)

#+test
(xraw-learn)

#+test
(dump-text "/home/data/ReSource3/Resource3_051507.txt")

(defstruct (source-header-info (:conc-name nil))
  source-to-pq-columns column-label tags-col 
  taxonomynode-col header-itags column-count)

(defstruct header
  source-headers
  std-headers
  column)

(defun item-col (h id)
  (gethash id (header-column h)))

(defparameter *header-info* nil)

(defun item-col* (id)
  (gethash id (header-column *header-info*)))

(defmacro itmcol (id)
  `(item-col header-info ',id))



(defmacro itmcol* (id)
  `(item-col *header-info* ',id))

(defparameter *tag-separators* ",;")
(defparameter *tag-separator-in-use* #\,)

(defmacro itm-f (idvar)
  `(aref item-fields (item-col* ,idvar)))

(defmacro chg-f (idvar)
  `(aref change-reasons (itmcol ,idvar)))

(defmacro chg-f* (idvar)
  `(aref change-reasons (item-col* ,idvar)))

(defmacro itm-f? (idvar)
  (let ((idx (gensym)))
    `(bwhen (,idx (item-col* ,idvar))
       (aref item-fields ,idx))))

(defmacro itmf? (idsym)
  (let ((idx (gensym)))
    `(bwhen (,idx (gethash ',idsym (header-column *header-info*)))
       (aref item-fields ,idx))))

(defmacro itmf (idsym)
  `(aref item-fields (gethash ',idsym (header-column *header-info*))))

(define-symbol-macro .resitnum (itmf? resitnum))
(define-symbol-macro .noun (itmf? noun))
(define-symbol-macro .type (itmf? type))
(define-symbol-macro .venname (itmf? venname))
(define-symbol-macro .normvenname (itmf? normvenname))
(define-symbol-macro .vencatnum (itmf? vencatnum))
(define-symbol-macro .itdesc (itmf? itdesc))
(define-symbol-macro .tags (itmf? tags))
(define-symbol-macro .trademark (itmf? trademark))
(define-symbol-macro .taxonomynode (itmf? taxonomynode))

#+test
(source-dump-headers (make-data-raw-pathname "MASTER_NT_ONLY_18980_060507.csv") #\,)

#+test
(source-dump-headers *raw* #\tab)

#+test
(source-dump-headers "d:/0ghx/r2.txt")

(defparameter *known-tags* nil)

(defparameter *pq-tag-symbols-in-order*
  (apply 'vector '(trademark composition age gender size 
                    primary-size secondary-size outer-diameter volume weight
                    ergonomic-location properties sterility latexity disposability
                    color flavor fragrance hazardous other)))

(defparameter *pq-tags-symbolic*
  '(noun type trademark composition age gender size 
     primary-size secondary-size outer-diameter volume weight
     ergonomic-location properties sterility latexity disposability
     color flavor fragrance hazardous other))

(defun pq-tag-symbol-to-encoded-string (pq-symbol)
  (or (get pq-symbol 'tags-id)
    (setf (get pq-symbol 'tags-id)
      (case pq-symbol
        (noun "N")
        (type "T")
        (otherwise (princ-to-string (1- (position pq-symbol *pq-tags-symbolic*))))))))

#+test
(mapcar 'pq-tag-symbol-to-encoded-string '(noun trademark))

(defparameter *pq-ghx-header-xlate*
  '((trademark . trademark_brandname)
    
    (trademark . trademark/brandname)
    
    (other . misc)
    (size . size_shape)
    (size . size/shape)
    (primary-size . primarylwh)
    (secondary-size . secondarylwh)
    (outer-diameter . diameter)
    (ergonomic-location . location)
    (sterility . sterile_nonsterile)
    (latexity . latex_latexfree)
    (disposability . disposable_reusable)))

(defun ghx-tag-symbolic (pq-tag-symbolic)
  (when (stringp pq-tag-symbolic)
    (setf pq-tag-symbolic (intern (string-upcase pq-tag-symbolic) :wordnet)))
  (or (cdr (assoc pq-tag-symbolic *pq-ghx-header-xlate*))
    pq-tag-symbolic))

(defun pq-tag-symbolic (ghx-tag-symbolic)
  (when (stringp ghx-tag-symbolic)
    (setf ghx-tag-symbolic (intern (string-upcase ghx-tag-symbolic) :wordnet)))
  (or (car (rassoc ghx-tag-symbolic *pq-ghx-header-xlate*))
    ghx-tag-symbolic))

#+test
(pq-tag-symbolic "color")

(defparameter *measure-tags* '(outer-diameter volume weight))

(defun string-to-tag (s)
  "assumes valid string, tho with case insensitivity on N and T"
  (case (schar s 0)
    ((#\n #\N) 'noun)
    ((#\t #\T) 'type)
    (otherwise (let ((n (parse-integer s :junk-allowed t)))
                 (when (< 0 n 21)
                   (aref *pq-tag-symbols-in-order* (1- n)))))))
                    
;;; --- utitlities -------------------------------

(defun spreadsheet-cell-clean (row start end)
  (spreadsheet-value-clean (subseq row start end)))

(defun spreadsheet-value-clean (value)
  (unless (string= value "")
    (setf value (string-trim '(#\space) value))
    (setf value (string-trim '(code-char 160) value))
    (unless (string= value "")
      (cond
       ((and (char= #\" (schar value 0))
          (> (length value) 1)
          (char= #\" (schar value (1- (length value)))))
        (setf value (spreadsheet-value-clean (subseq value 1 (1- (length value))))))
       ((char= #\" (schar value 0))
        (setf value (spreadsheet-value-clean (subseq value 1))))
       ((let ((last (1- (length value))))
          (when (and (plusp last) (char= #\" (schar value last)))
            (unless (digit-char-p (schar value (1- last)))
              (setf value (spreadsheet-value-clean (subseq value 0 last))))))))))
    value)

#+test
(mapcar 'spreadsheet-value-clean 
  ;;'("5\"") #+not
  '("" "a"  "a " "5\"" "\"5\"" "\" 5 \" " "a\"" "\"" "MEDTRONIC"))

(defun taxonomy-dump (source-pathname taxonomy &optional (delimiter #\tab))
  (with-open-file (source source-pathname :direction :input)
    (let* ((header (read-line source :eof))
           (tax-pos (position$ "taxonomynode8" (mapcar 'trim$ (ukt:split-sequence delimiter header))))
           (itdesc-pos (position$ "itdesc" (mapcar 'trim$ (ukt:split-sequence delimiter header))))
           (tags-pos (position$ "tags" (mapcar 'trim$ (ukt:split-sequence delimiter header)))))
      (assert tax-pos () "taxo not in ~a" (mapcar 'trim$ (ukt:split-sequence delimiter header)))
      (assert tags-pos () "tags not in ~a" (mapcar 'trim$ (ukt:split-sequence delimiter header)))
      (assert itdesc-pos () "itdesc-pos not in ~a" (mapcar 'trim$ (ukt:split-sequence delimiter header)))
      (loop for sample-data = (mapcar 'trim$ (ukt:split-sequence delimiter (read-line source :eof)))
            for x below 100000
          when (eql 0 (search taxonomy (nth tax-pos sample-data)))
          do (format t "~&~{~a~^ ~}" (mapcar (lambda (n) (nth n sample-data))
                                       (list tax-pos itdesc-pos tags-pos)))
            ))))

#+test
(taxonomy-dump "d:/0ghx/r2.txt" "42272209")

(defun tags-itags-combine (item-fields change-reasons)
  ;
  ; as of 7/2007 ReSource files redundantly bear both an aggregate tag-string "n=hat,type=top,..." and
  ; distinct columns per tag (ITAGs). combine them here, gleaning from the tag-string only absent a 
  ; corresponding ITAG.
  ;
  ;
  ; first repair itag if necessary, since they can be miscoded as can be aggregate tag strings
  ;

  (when .tags
    (loop for (id . values) in (tag-string-parse .tags)
        for col-no = (item-col* id)
        when (and  values (not (itm-f id)))
          ;; there is an itag column in the data, we have tags values for it, and the column is blank in this item
        do
          (log-value-incf 'itag-columns-extended-from-tags)
          (log-value-incf 'itag-values-added-from-tags (length values))
          (when (not (listp values)) ;; bug somewhere
            (setf values (list values)))
          (setf (itm-f id) values)
          (when change-reasons
            (push :fill-from-tags (aref change-reasons col-no))))))
  
(defun item-fields-to-tag-string (item-fields)
  (with-output-to-string (s)
    (loop for first = t then nil
        for id in *pq-tags-symbolic*
        for tag-code = (pq-tag-symbol-to-encoded-string id) do
          (bwhen (values (itm-f id))
            (unless (and (stringp values)(string= values ""))
              (unless first
                (princ *tag-separator-in-use* s)
                (princ #\space s))
              (princ tag-code s)
              (princ #\= s)
              (etypecase values
                (string (princ values s))
                (cons (format-tag-values s id values))))))))

(defun format-tag-values (stream tag-sym-id values)
  (format-string-delimited-list stream values (case tag-sym-id ((noun type) "/") (otherwise "; "))))
