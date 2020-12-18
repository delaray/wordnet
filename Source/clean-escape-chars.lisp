;; (eval-when (compile load eval)
;;   (require :cg) )

#-default-pkg
(in-package :wordnet)

;;: SPEC-STRING
(defstruct (SPEC-STRING (:conc-name  SPST-))
  sort-key	; fixnum
  sort-key2	; simple-string
  sort-key3	; fixnum
  key		; fixnum or simple-string
  value		; simple-string to replace with or one of :unknown, :leave.
  sample	; simple-string with html-tags inside
  comment	; simple-string or nil.
  files		; list of cons (file-number . file-name)
  count )	; integer. The number of occurrences of the code in the all files processed.

;;: html-encode
(defun html-encode (txt)
  (excl:replace-re (excl:replace-re (excl:replace-re txt "&" "&amp;") "<" "&lt;") ">" "&gt;") )


;;: *ACODE*
(defconstant *ACODE*
  (excl:compile-re
  #.(concatenate 'string
     "^(?:"
      "&#x[\\da-fA-F]{1,4}(?![\\da-fA-F]);?"           "|"
      "&#(\\d{1,5})(?!\\d);?"                          "|"	; nmb
      "&(\\d{1,5});"                                   "|"	; nmb2
      "&(\\d{3,5})(?!\\d)"                             "|"	; nmb3
      "&[a-z]{1,16};"                                  "|"
      "&[a-z]{1,16}_?"                                 "|"
;      "&[a-z]{1,16}_?(?![\\da-zA-Z_]*=)"               "|"
      "%[a-z]{1,16};?"                                 "|"
      "(</?[a-zA-Z]{1,16})(?:/| [^<>\\x09]{,100})?>"   "|"	; tag
      "\\\\[a-zA-Z]{1,16}"
     ")" )))

;;: decode-string
;;	col		string to be decoded.
;;	dict		hash-table :test #'equal. Will be modified.
;;	dlist		:no-stats or list of SPEC-STRING.
;;	file-ptr	nil or cons: (file-number . input-file-name).
;;	test-only	boolean. t means "do not return decoded col".
;; Returns:
;;	list:		the modified dlist.
;;	nil or string:	the decoded col (always nil when test is not t).
(defun decode-string (col dict dlist file-ptr test-only)
  (let ((col-len (length col))
        (i       0)
        (prv     0)
        mod-col )
    (declare (fixnum col-len i prv))
    (loop while (< i col-len)
          for c of-type character = (char col i)
          for nxt fixnum = (1+ i)
          for key        = nil
          for sort-key   = nil
          for sort-key2  = nil
      do
      (if (or (char= c #\&) (char= c #\<) (char= c #\%) (char= c #\\))
        (multiple-value-bind (rc whole nmb nmb2 nmb3 tag)
                             (excl:match-re *ACODE* col :start i :end col-len :return :index)
          (when rc
            (unless nmb
              (setq nmb (or nmb2 nmb3)) )
            (setq key       (if tag (nstring-downcase (concatenate 'string (subseq col (car tag) (cdr tag)) ">"))
                                    (subseq col (car whole) (cdr whole)) )
                  sort-key  (if nmb (parse-integer col :start (car nmb) :end (cdr nmb)) 10000000)
                  sort-key2 key
                  nxt       (cdr whole) )))
        (when (or (> (char-code c) 127)
                  (< (char-code c) 32) )
          (setq key       (char-code c)
                sort-key  key
                sort-key2 "" )))
      (when key
        (let* ((itm (gethash key dict))
               (val (when itm (SPST-value itm))) )
          (when (and (not val) (fixnump sort-key) (> sort-key 31) (< sort-key 128))
            (setq val (make-string 1 :initial-element (code-char sort-key))) )
          (when val
    	    (unless (or (eq val :unknown) (eq val :leave))
              (unless test-only
                (setq mod-col (concatenate 'string (or mod-col "") (subseq col prv i) val))
                (setq prv nxt) )))
    	  (unless (or val itm (char= #\\ c)	; no value in the dictionary and the code is not '\' or a simple one (0..127)
                              (and (fixnump key) (< key 128)) )
    	    (setq itm
                  (make-SPEC-STRING :sort-key  sort-key
                                    :sort-key2 sort-key2
                                    :sort-key3 (if (integerp key) 0 1)
                                    :count     0
                                    :key       key		; fixnum or simple-string
                                    :value     :unknown ))	; simple-string to replace with or one of :unknown, :leave
            (when (listp dlist)
              (push itm dlist) )
    	    (setf (gethash key dict) itm) )
    	  (when itm
            (unless (SPST-sample itm)
              (setf (SPST-sample itm)
                    (concatenate 'string
                      (html-encode (subseq col (max 0 (- i 30)) i))
                      "<SPAN style='background-color:yellow'>"
                      (html-encode (subseq col i nxt))
                      "</SPAN>"
                      (html-encode (subseq col nxt (min (+ nxt 20) col-len))) )))
            (when file-ptr
              (unless (eq file-ptr (first (SPST-files itm)))
                (push file-ptr (SPST-files itm)) ))	; list of files
            (incf (SPST-count itm)) )		; integer. The number of occurrences of the code in the all files processed.
          (when (and (not val) (char= #\< c))
            (setq nxt (1+ i)) )))
      (setq i nxt) )
    (when (and (plusp prv) (< prv i))
      (setq mod-col (concatenate 'string mod-col (subseq col prv i))) )
    (values dlist mod-col) ))


(defconstant *XCODE* (excl:compile-re "^(?:&(?:#?(?:\\d{1,6}|x[\\da-f]{4})|[a-z]{1,16})[;_]?|%[a-z]{1,16};?|</?[a-z]{1,16} ?/?>|\\d{1,6}|\\\\[a-zA-Z]{1,16})$"))

;;: read-convert-dict
;;	dict			hash-table :test #'equal
;;	convert-dict-path	string
;; Returns:
;;	list of SPEC-STRING.
(defun read-convert-dict (dict convert-dict-path)
  (let ((col-idxs '((0 0) (1 1) (2 2)))
        (columns   (make-array 3))
        dlist )
    (setq col-idxs (reduce #'append (sort (copy-list col-idxs) #'< :key #'first)))	; ((14 1) (12 2) (13 0)) => (12 2 13 0 14 1)
    (check-COLUMN-IDXS col-idxs)
    (flet ((f-process-line (line line-len)
                           (declare (ignore line-len))
                           (fill columns "")
                           (tokenizer-by-deliminators line '(#\Tab #\Newline #\Return)
                                                      :discard     nil
                                                      :table       col-idxs
                                                      :dest-vector columns )
                           (let ((key (aref columns 0))
                                 (val (aref columns 1))
                                 (cmt (aref columns 2))
                                 sort-key sort-key2 )
                             (setq sort-key  10000000
                                   sort-key2 key )
                             (when (excl:match-re *XCODE* key :return :index)
                               (if (digit-char-p (char key 0))
                                 (setq key      (parse-integer key)
                                       sort-key  key
                                       sort-key2 "" )
                                 (when (or (char= #\& (char key 0)) (char= #\% (char key 0)))
                                   (if (and (> (length key) 1) (digit-char-p (char key 1)))
                                     (setq sort-key  (parse-integer key :start 1 :junk-allowed t)
                                           sort-key2 "" )
                                     (when (and (> (length key) 2) (digit-char-p (char key 2)))
                                       (setq sort-key  (parse-integer key :start 2 :junk-allowed t)
                                             sort-key2 "" )))))
                               (when (string= val "blank")
                                 (setq val "") )
                               (when (string= val "space")
                                 (setq val " ") )
                               (when (string= val "leave")
                                 (setq val :leave) )
                               (if (gethash key dict)
                                 (format t "~&~A is duplicated in the dictionary~%" key)
                                 (let ((itm (make-SPEC-STRING :sort-key  sort-key
                                                              :sort-key2 sort-key2
                                                              :sort-key3 (if (integerp key) 0 1)
                                                              :count     0
                                                              :comment   cmt
                                                              :key       key		; fixnum or simple-string
                                                              :value     val )))	; simple-string to replace with or one of :unknown, :leave
                                   (setf (gethash key dict) itm)
                                   (push itm dlist) ))))
                           t ))
      (process-file-by-lines convert-dict-path  #'f-process-line
                             :external-format *EXTERNAL-FORMAT*
                             :bypass          1		; bypass a header line
                             :buffer-size     1000 ))
    dlist ))


