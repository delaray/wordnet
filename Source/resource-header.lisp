(in-package :wordnet)

;;; --- header madness -----------------------------------
(defparameter *temp-headers*
  '((ITEM_ID |ALLSOURCE DIVISION| DIVORG PARENTORG |ALLSOURCE PART NUMBER| |ALLSOURCE UOM|
      |ALLSOURCE QOE| ALLSOURCE_DESC LOCALE_CODE |SUPPLIER PARENT COMPANY NAME| PARTNOSTATUS
      ITEMSTATUS)
    (ITEM_ID |ALLSOURCE DIVISION| DIVORG PARENTORG |ALLSOURCE PART NUMBER| |ALLSOURCE UOM|
      |ALLSOURCE QOE| ALLSOURCE_DESC LOCALE_CODE PARTNUMBERTYPE PARTNOSTATUS ITEMSTATUS)))

(defparameter *delim-headers*
  '((ITEM_ID |ALLSOURCE DIVISION| DIVORG PARENTORG |ALLSOURCE PART NUMBER| |ALLSOURCE UOM| |ALLSOURCE QOE| 
      (ALLSOURCE_DESC . itdesc) LOCALE_CODE |SUPPLIER PARENT COMPANY NAME| PARTNOSTATUS ITEMSTATUS)
    (ITEM_ID |ALLSOURCE DIVISION| DIVORG PARENTORG |ALLSOURCE PART NUMBER| |ALLSOURCE UOM| |ALLSOURCE QOE| 
      (ALLSOURCE_DESC . itdesc) LOCALE_CODE PARTNUMBERTYPE PARTNOSTATUS ITEMSTATUS)
    (resitnum venname normvenname vencatnum mfgname normmfgname mfgcatnum ghxmfgcatnum
      ghxuom ghxqoe itdesc tags taxonomynode
      noun
      type
      (trademark_brandname . trademark)
      composition
      age
      gender
      (size_shape . size)
      (primarylwh . primary-size)
      (secondarylwh . secondary-size)
      (diameter . outer-diameter)
      volume
      weight
      (location . ergonomic-location)
      properties
      (sterile_nonsterile . sterility)
      (latex_latexfree . latexity)
      (disposable_reusable . disposability)
      color
      flavor
      fragrance
      hazardous
      (misc . other)
      partqastatus
      transqastatus )
    
     
    (resitnum venname normvenname vencatnum mfgname normmfgname mfgcatnum ghxmfgcatnum
      ghxuom ghxqoe itdesc tags (taxonomynode8 . taxonomynode)
      (catlevel1_8 . catlevel1)(catlevel2_8 . catlevel2)(catlevel3_8 . catlevel3)(catlevel4_8 . catlevel4)
      noun
      type
      (trademark_brandname . trademark)
      composition
      age
      gender
      (size_shape . size)
      (primarylwh . primary-size)
      (secondarylwh . secondary-size)
      (diameter . outer-diameter)
      volume
      weight
      (location . ergonomic-location)
      properties
      (sterile_nonsterile . sterility)
      (latex_latexfree . latexity)
      (disposable_reusable . disposability)
      color
      flavor
      fragrance
      hazardous
      (misc . other)
      partqastatus
      transqastatus )
    (noun type commodity uniqueid taxonomynode |BRAND NAME| vendor)
    (pof_vencat_num pof_vendor match_type match_method 
      pof_itdesc it_descr_it_desc qa n t |1| unspc)
    
    (line_no row_id match_type match_method seconday_elmt pof_vencat_num pof_vendor pof_itdesc
      iid_template 
      (ven_cat_num . vencatnum) rs_ven_cat_num 
      (it_desc . itdesc) r_it_desc 
      (ven_family_name . normvenname)
      (vendor . venname) res_vendor
      tags idd_tags pof qa 
      (n . noun) pof_n  
      (t . type) pof_t  
      (|1| . trademark) pof_1  
      (|2| . composition) pof_2  
      (|3| . age) pof_3 
      (|4| . gender) pof_4 
      (|5| . size) pof_5  
      (|6| . primary-size) pof_6  
      (|7| . secondary-size) pof_7  
      (|8| . outer-diameter) pof_8  
      (|9| . volume) pof_9  
      (|10| . weight) pof_10 
      (|11| . ergonomic-location) pof_11 
      (|12| . properties) pof_12  
      (|13| . sterility) pof_13  
      (|14| . latexity) pof_14  
      (|15| . disposability) pof_15  
      (|16| . color) pof_16  
      (|17| . flavor) pof_17  
      (|18| . fragrance) pof_18  
      (|19| . hazardous) pof_19  
      (|20| . other) pof_20 
      pof_u  
      |22| pof_22 qa_22
      (mfg_cat_num . mfgcatnum)
      (mfg_name . mfgname)
      (mfg_family_name . mfgfamilyname)
      |NUVIA VENDOR|
      (uom . ghxuom)
      (qoe . ghxqoe)
      (unspc . taxonomynode))))

(defun delim-header-seek (source-line
                          &key
                          (delimiter #\tab)
                          (end (length source-line))
                          (expected-headers *delim-headers*)
                          (dbg nil))
  "look for header info, defined as a data record with known col headers.
Returns header-info structure, including mapping of col-no to standard name for column"
  (let ((source-headers (loop for h in (split-sequence delimiter source-line :end end)
                            collecting (intern (up$ (trim$ h)) :wordnet))))
    (loop
      for expected-header in expected-headers
      for expected-source-headers = (loop for h in expected-header
                                        collecting (if (consp h) (car h) h))
      for std-headers = (loop for h in expected-header
                            collecting (if (consp h) (cdr h) h))
      thereis
      (progn
        (when dbg
          (trc  "exp" expected-source-headers)
          (trc  "got" source-headers))
        (bwhen (matches (intersection expected-source-headers source-headers))
          (bif (missing (set-difference expected-source-headers source-headers))
            (when nil ;; t ;; dbg
              (trc  "--- start ------------------" )
              (trc  "ignoring partial header match" expected-header)
              (trc  "partial match missing" missing)
              (trc  "got only" matches)
              (trc  "unused headers" (set-difference source-headers matches))
              (trc  "--- end ------------------" )
              #+happens (break "expecting partial? see console for diagnostics"))
            (progn
              (trc "BINGO header:" expected-header)
              (bwhen (extras (set-difference source-headers expected-source-headers))
                (trc "header extras OK" extras))
              (return (make-header
                       :source-headers source-headers
                       :std-headers (loop for h in source-headers
                                        for col-no upfrom 0
                                        for pos = (position h expected-source-headers)
                                        collecting (when pos (elt std-headers pos)) into stds
                                        finally (return (apply 'vector stds)))
                       :column (loop with ht = (make-hash-table)
                                   for h in source-headers
                                   for col-no upfrom 0
                                   for pos = (position h expected-source-headers)
                                   when pos
                                   do (setf (gethash (elt std-headers pos) ht) col-no)
                                   finally (return ht)))))))))))

(defparameter *known-headers*
  '((resitnum venname ghxvenname mfgname vencatnum itdesc tags taxonomynode8)
    (resitnum venname normvenname mfgname vencatnum itdesc tags taxonomynode)
    (resitnum venname ghxvenname mfgname vencatnum itdesc tags)
    (nil |ALLSOURCE DIVISION| nil nil |ALLSOURCE PART NUMBER| allsource_desc) ;; gotta be in same order as standard
    (nil |VENDOR NAME| nil nil |VENDOR CATALOG NUMBER| |ITEM DESCRIPTION|)
    (|NOUN| |TYPE| |COMMODITY| |UNIQUEID| |TAXONOMYNODE|)))


(defparameter *standard-tag-id*
  '(noun type trademark composition age gender size 
     primary-size secondary-size outer-diameter volume weight
     ergonomic-location properties sterility latexity disposability
     color flavor fragrance hazardous other))


#+test
(source-dump-headers "/0ghx/rawdata/r3-25k.txt" #\tab)

(defparameter *known-desc-tag-headers*
  '((itdesc tags) ;; Rn, and our standard
    (allsource_desc) ;; gotta be in same order as standard
    (|ITEM DESCRIPTION|)))

(defparameter *panic-headers*
  '((resitnum  vencatnum itdesc tags)))

#+test
(xraw-learn)

(defun header-info-seek (source-line &key (delimiter #\tab) (end (length source-line)) (expected-headers *known-headers*))
  "look for header info, defined as a data record with known col headers, return assoc of col-label and position"
  ;;
  ;; this function is a little brittle and will likely need improvement to handle tougher cases
  ;;
  
  (let ((col-headers (loop for tab-no upfrom 0
                           for hdr in (mapcar 'trim$
                                        (ukt:split-sequence delimiter source-line :end end))
                           collecting (if (empty$ hdr)
                                          (format nil "ANON~3'0',,a" tab-no)
                                        hdr))))
    
    (trc  "found headers" col-headers)

    (loop for known-header in expected-headers
        for mappings = (loop for known-header-col in known-header
                           collecting (if (eq known-header-col :skip)
                                          :skip
                                          (position known-header-col col-headers :test 'string-equal)))
        if (notany #'null mappings)
        do (trc "using known header" known-header)
          (return (make-source-header-info
                    :column-count (length col-headers)
                    :column-label (apply 'vector col-headers)
                    :source-to-pq-columns mappings
                    :tags-col (bwhen (p (position 'tags known-header))
                                (unless (fixnump (nth p mappings))
                                  (break "bad col ~a ~a" p mappings))
                                (nth p mappings))
                    :taxonomynode-col (bwhen (p (position 'taxonomynode known-header)) ;; I DOUBT THIS WILL HOLD UP, MIGHT BE "UNSPSC" INSTEAD
                                        (nth p mappings))
                    :header-itags (loop initially (trc "hdrs" col-headers)
                                        with table
                                      for col-header in col-headers
                                      for pq-col-header = (pq-tag-symbolic col-header)
                                      for col-no upfrom 0
                                      if (find pq-col-header *pq-tags-symbolic*)
                                      do (setf table (or table (make-hash-table)))
                                        (setf (gethash pq-col-header table) col-no)
                                        (trc nil "itag col" pq-col-header col-no)
                                      finally (return table))))
        else if (notevery #'null mappings)
        do ;(print `(source ,source-line))
          ;(print `(known-partially-mapped ,known-header))
          ;(print `(mappings ,mappings))
          (trc nil "ignoring partial header match -- cool? -- see listener for deets"))))

(defun delim-header-clean (s col-no)
  (declare (ignorable col-no))
  (let ((trim (trim$ s)))
    trim
    #+nahhhh (if (empty$ trim)
                 (format nil "TAB~3,,,'0@a" col-no)
               trim)))

#+test
(list
 (delim-header-clean "vencatnum" 42)
 (delim-header-clean "  " 42))


(defun hash-table-assoc (ht &optional (show-blank? t) sort-key)
  (let ((a (loop for hk being the hash-keys of ht
               using (hash-value hv)
                 
               when (or show-blank?
                      ;(trc "hv" hv (not (string= hv "")))
                      (etypecase hv
                        (symbol t)
                        (string (not (string= hv "")))
                        (cons (loop for hvi in hv
                                  thereis (not (string= hvi ""))))))
               collecting (cons hk hv))))
    (if sort-key
        (sort a '< :key sort-key)
      a)))

#+test
(test-delim-header.seek)

#+test
(defun test-delim-header.seek ()
  (with-open-file (source *raw* :direction :input :external-format *EXTERNAL-FORMAT*)
    (let ((line1 (read-line source :eof)))
      (bif (hi (delim-header-seek line1
;;;                 :expected-headers '((LINE_NO ROW_ID MATCH_TYPE MATCH_METHOD SECONDAY_ELMT POF_VENCAT_NUM POF_VENDOR
;;;                                       POF_ITDESC IID_TEMPLATE VEN_CAT_NUM RS_VEN_CAT_NUM IT_DESC 
;;;                                       R_IT_DESC VEN_FAMILY_NAME
;;;                                       VENDOR RES_VENDOR TAGS IDD_TAGS POF QA N POF_N QA_N 
;;;                                       T POF_T QA_T |1| POF_1 QA_1 |2|
;;;                                       POF_2 QA_2 |3| POF_3 QA_3 |4| POF_4 QA_4 |5| POF_5 QA_5 
;;;                                       |6| POF_6 QA_6 |7| POF_7 QA_7
;;;                                       |8| POF_8 QA_8 |9| POF_9 QA_9 |10| POF_10 QA_10 |11| POF_11 QA_11 |12| POF_12 QA_12
;;;                                       |13| POF_13 QA_13 |14| POF_14 QA_14 |15| POF_15 QA_15 |16| POF_16 QA_16 |17| POF_17
;;;                                       QA_17 |18| POF_18 QA_18 |19| POF_19 QA_19 |20| POF_20 QA_20 U POF_U QA_U |22| POF_22
;;;                                       QA_22 MFG_CAT_NUM MFG_NAME MFG_FAMILY_NAME |Nuvia Vendor| 
;;;                                       UOM QOE UNSPC) )
                 ))
        (trc "bingo headers" (hash-table-assoc (header-column hi)))
        (trc "nope")))))

(defun test-header-info-seek ()
  (with-open-file (source "C:/0ghx/Data/GHX-Data/Reference-data/AllSource/3MHealthCare.txt"
                    #+works "C:/0ghx/Data/GHX-Data/Reference-data/R1/Resource_Project_1_06272006_1.txt" :direction :input
                     :external-format *EXTERNAL-FORMAT*)
      (let* ((line1 (read-line source :eof))) ;; might be header, might not
        (print (header-info-seek line1)))))

#+dumpi
(source-dump-headers #p"/home/data/ReSource2/resource_complete_export_011006.txt.7_1")

#+dumpr3
(source-dump-headers *raw*)

#+dumpr2
(source-dump-headers "~/krez/oos_conv_110907.csv" #\tab)

(defun source-dump-headers (source-pathname &optional (delimiter #\tab))
  (with-open-file (source source-pathname :direction :input :external-format *EXTERNAL-FORMAT*)
    (loop with header = (prog1
			    (read-line source :eof)
			  (print :got-line-one))
	with col-headers = (mapcar 'trim$ (ukt:split-sequence delimiter header))
        with data-1 = (prog1
			  (read-line source :eof)
			(print :got-line-two))
	with sample-datums = (mapcar 'trim$ (ukt:split-sequence delimiter data-1))
        for col-no below (max (length col-headers)(length sample-datums))
        for col-header = (nth col-no col-headers) 
        for sample-data = (nth col-no sample-datums)
          
        do
          ;(inspect header)
          ;;(print (list col-no col-header sample-data))
          ;;  (loop for ch in col-header do (print `(chdr ,ch)))
          (format t "~&~d ~5t~a~25t: ~a" col-no col-header sample-data)
	  (format t "~&~{~d ~}" (loop for c across col-header collecting (char-code c)))
          collect (intern (string-upcase col-header)) into hdrs
        finally (trcx dumped (length header)(length data-1))
          (return hdrs))))

(defun source-dump-headers-when (source-pathname test &optional (delimiter #\tab))
  (with-open-file (source source-pathname :direction :input :external-format *EXTERNAL-FORMAT*)
    (let* ((header (read-line source :eof))
           (col-headers (mapcar 'trim$ (ukt:split-sequence delimiter header))))
      (loop for sample-data = (mapcar 'trim$ (ukt:split-sequence delimiter (read-line source :eof)))
          when (funcall test col-headers sample-data)
            do (loop for col-no upfrom 0
                   for col-header in col-headers
                   for sample-datum in sample-data
                   do (format t "~&~a~20t: ~a" col-header sample-datum))
            
            (loop-finish)))))

#+test
(source-dump-headers-when "d:/0ghx/r2.txt"
  (lambda (h d)
    (declare (ignorable h d))
    (string-equal (car d) "580373")))

