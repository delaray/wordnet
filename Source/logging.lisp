(in-package :wordnet)

(defvar *log*)

(def-db cleaning-log *cleaner-data-directory*
    :use :disk
  :class-cache-size (* 50 1024 1024)
  :object-cache-size 11000)

(defmacro with-log-db (&body code)
  `(with-db (*cleaning-log*)
     ,@code))

(defun make-log-entry (type &rest initargs)
  (cleaning-log-open-ensure :if-does-not-exist :create)
  (with-log-db
      (apply 'make-instance type initargs)))

#+test
(cleaning-log-create)

#+ttest
(with-log-db
    (doclass (c 'raw-log)
      (describe c)))

(defclass* raw-log (:print nil)
  input-file-pathname
  (log-universal-time :initform (get-universal-time))
  (items-read :initform 0)
  (part-qa-status-low :initform 0)
  (part-qa-status-11-12 :initform 0)
  (distinct-tag-words :initform 0)
  (distinct-noun-types :initform 0)
  (distinct-noun-types-to-reverse :initform 0)
  (trivial-distinct-noun-types-to-reverse :initform 0)
  (apostrophes-removed :initform 0)
  (invalid-taxonomy-node :initform 0)
  (taxo-minus-1 :initform 0)
  (slash-treated-as-semicolon :initform 0)
  (tag-chars-cleaned :initform 0)
  (tag-chars-not-cleaned :initform 0)  
  (TAG-PAIR-ID-ONLY :initform 0)
  (TAG-PAIR-ID-MISSING :initform 0)
  (TAG-PAIR-UNJOINED-OK :initform 0)
  
  (tag-pair-unsalvageable :initform 0)
  (itag-with-comma-char :initform 0)
  (itag-with-equal-char :initform 0)
  (tags-itags-resolved :initform 0)
  (itag-columns-extended-from-tags :initform 0)
  (itag-values-added-from-tags :initform 0)
  )


(defclass* tag-repair-log (:print nil)
  input-file-pathname
  (log-universal-time :initform (get-universal-time))
  (spelling-fixes :initform 0)
  (formatting-fixes :initform 0)
  (attributes-retagged :initform 0))

(defclass* cleaning-log (raw-log :print nil)
  input-file-pathname
  output-file-pathname
  (log-start-time :initform (get-universal-time))
  log-end-time
  (items-read :initform 0)
  (items-fixed :initform 0)
  (noun-type-reversal-items :initform 0)
  itdesc-to-tags
  (map-by-word-fixes :initform 0)
  (map-fixes :initform 0)
  (tag-to-tag-moves :initform 0)
  (tag-to-tag-shares :initform 0)
  (mistaggings-avoided :initform 0)
  (taxo-orig :initform 0)
  (taxo-changed :initform 0)
  (taxo-blank-filled :initform 0)
  (taxo-obs-saved :initform 0)
  (taxo-minus-one-filled :initform 0)
  (taxo-unknown-saved :initform 0)
  (both-ven-fields :initform 0)
  (both-mfg-fields :initform 0) ;-
  (mfg-name :initform 0) ;-
  (mfg-cat-num :initform 0) ;-
  (both-uom-fields :initform 0) ;-
  (nouns :initform 0)
  (types :initform 0)
  (both-noun-type-fields :initform 0)
  (venname-cleaned :initform 0)
  (normvenname-cleaned :initform 0)
  (mfgname-cleaned :initform 0)
  (mfgfamilyname-cleaned :initform 0)
  changed-by-column
  filled-by-column
  erased-by-column)

;;;*	Count of records by Txn Vendor Name fields
;;;*     Count of records by TXN Vendor Item IDs
;;;*     Count of records by TXN Vendor Name and Vendor Item both present
;;;in the same record
;;;*	Count of records by Txn MFg Name 
;;;*     Count of records by TXN Mfg Item IDs
;;;*     Count of records by TXN Mfg Name and Mfg Item both present in the
;;;same record
;;;*     Count of records by TXN MFG Name and MFg Item only 1 present in
;;;the same record
;;;*	Count of records by Txn UOM fields present
;;;*     Count of records by Txn QOE field present
;;;*     Count of records by TXn UOM & QOE both present in the same record
;;;*     Count of records by Txn UOM & QUO either present in the same
;;;record



