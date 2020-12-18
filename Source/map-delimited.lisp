(in-package :wordnet)


;;: *ESCAPE-SYMBOLS-DICTIONARY*		hash-table :test #'equal
(defvar *ESCAPE-SYMBOLS-DICTIONARY* nil)

#+test
(source-dump-headers *raw*)

;;: map-delimited
;;	source-pathname		path
;;	header-info		struct 'header
;;	map-fn			designator for a function of four arguments: old-line line-len header-info item-fields
;;	start			integer
;;	end			nil or integer: a max number of lines to be processed
;;	verbose			boolean
;;	partial-handling	:error, :process, or :ignore
;;	short-handling		:error, :process, or :ignore
;;	long-handling		:error, :process, or :ignore
;;	parse-known-tags?	boolean
;;	headers
(defun map-delimited (source-pathname header-info
                       &key map-fn (start 0) end (verbose nil)
                            (partial-handling :process)
                            (short-handling   :process)
                            (long-handling    :process)
                            (parse-known-tags? t)
                            headers )
  (declare (ignorable verbose))
  (let (;(source-id (intern (last1 (pathname-directory source-pathname)) :keyword))
        ;(source-date (file-write-date source-pathname))
        (item (make-string 9192))
        (start-time (now)))
    (declare (ignorable start-time))
    (with-open-file (source source-pathname :direction :input :external-format *EXTERNAL-FORMAT*)
      (let* ((line1-len (multiple-value-bind (len reason)
                                             (read-line-into item source :eof)
                                             (assert (not (eq :short reason)) () "Make the buffer bigger: len ~a, reason ~a" len reason)
                                             len ))
             (local-header-info (delim-header-seek item
                                  :end line1-len
                                  :expected-headers (if headers
                                                      (list headers)
                                                      *delim-headers* )))
             (items-processed 0) )
        (when local-header-info
          ;;(when t (trc "headers!!!" (subseq item 0 132)))
          (setf line1-len nil) ;; so it does not get processed as data below
          (setf header-info local-header-info))
        ;(trc "source/header" source-pathname local-header-info)
        (assert header-info)
        (loop with *header-info* = header-info 
            with expected-column-count = (length (header-source-headers header-info))
            with expected-tab-ct = (1- expected-column-count)
            and item-fields = (make-array (length (header-source-headers header-info)))
            and short-line and short-tab-ct = 0
            and state = (if line1-len :process-line :need-line)
            and memory-no = 0
            and item = (make-string 9192) and item-len = line1-len and reason = nil and item-tab-ct
            until (or (eq state :fini)
                      (and end (eq state :need-line) (>= memory-no end)) )
            do
              (when short-line
                (trc-every (memory-no 1) "state ~a" state memory-no item-tab-ct ))
              (ecase state
                (:need-line		; a new line has to be read.
                 (incf memory-no)
                 (when (zerop (mod memory-no 10000))
                   (format t "~&Lines: ~A (~A)~%" memory-no source-pathname) )
                 (trc-every (memory-no 10000) "map-delim" memory-no (pathname-name source-pathname) 
                            :rate (floor memory-no (max 1 (- (now) start-time))) )
                 (multiple-value-setq (item-len reason)
                   (read-line-into item source nil :eof))
                 (when short-line
                   (trc "needline sees shortline and item-len" item-len :and-reason reason))
                 (setf state (case reason
                               (:eof :fini)
                               (:short (break "reason :short, item-len ~a: just deal with unpredictably long lins" item-len))
                               (t (setf item-tab-ct (count #\tab item :end item-len))
                                  (if short-line
                                    :work-on-short-line
                                    :evaluate-line )))))

                (:evaluate-line
                 (cond
                  ((> item-tab-ct expected-tab-ct)
                   (ecase long-handling
                     (:error
                       (break "excess columns in one line ~a over ~a in ~a" (1+ item-tab-ct) expected-column-count
                              (left$ item item-len) ))
                     (:ignore
                       (setf state :need-line) )
                     (:process
                       (setf state :process-line) )))
                  ((= item-tab-ct expected-tab-ct)
                   (setf state :process-line))
                  (t
                   (ecase partial-handling
                     (:error
                       (break "lack of columns in one line ~a instead of ~a in ~a" (1+ item-tab-ct) expected-column-count
                              (left$ item item-len) ))
                     (:ignore
                       (ecase short-handling
                         (:error
                           (break "short line. tabs ~a line ~a" short-tab-ct short-line) )
                         (:process
                           (setf state :process-line) )
                         (:ignore
		           (trc "short!!!!!" item-len item-tab-ct expected-tab-ct (count #\tab short-line))
                           (setf state :need-line) )))
                     (:process
                       (setf short-line   (subseq item 0 item-len)
                             short-tab-ct item-tab-ct
                             state        :need-line ))))))
                
                (:work-on-short-line
                 (trc "start on work on short line" item-len  item-tab-ct short-tab-ct )
                 (cond 
                  ((= (+ short-tab-ct item-tab-ct) expected-tab-ct)
                   (trc "completing short line with tabs" item-tab-ct :had short-tab-ct)
                   (setf short-line (conc$ short-line (subseq item 0 item-len)))
                   (loop for sc across short-line
                       for x upfrom 0
                       do (setf (schar item x) sc)
                       finally 
                         (setf (schar item (1+ x)) #\Return	; ???
                               item-len           (1+ x)
                               short-line         nil
                               state              :process-line )))
                    
                  ((or (= item-tab-ct expected-tab-ct)
                       (> (+ short-tab-ct item-tab-ct) expected-tab-ct) )
                   ;
                   ; we have a whole new line to worry about, not a completion of the short line
                   ;
                   (trc "wosl overflow" short-tab-ct item-tab-ct expected-tab-ct short-handling)
                   (ecase short-handling
                     (:error ; we have overflown the columns expected so we ain't rebuilding
                      (break "short line unsalvageable. tabs ~a line ~a"
                        short-tab-ct short-line))
                     (:process
                      (unless (< memory-no start)
                        (let ((short-len (length short-line)))
                          (incf items-processed)
                          (delim-fields-collect-into item-fields header-info short-line
                            :end short-len
                            :parse-known-tags? parse-known-tags?)
                          (trc  "processing partial tab ct" item-tab-ct :vs-expected expected-tab-ct)
                          (funcall map-fn short-line short-len header-info item-fields)
                          (setf short-line nil
                                state      :evaluate-line ))))
                     (:ignore
                      (setf short-line nil
                            state      :evaluate-line ))))

                  (t ;; still short
                   (trc "still short" short-tab-ct item-tab-ct expected-tab-ct (count #\tab item :end item-len))
                   ;(break "go?")
                   (when (plusp item-len)
                     (setf short-line (conc$ short-line (subseq item 0 item-len)))
                     (incf short-tab-ct item-tab-ct)
                     (trc "line is still short. got-tabs:" (count #\tab item :end item-len)
                       :need-tabs (1- expected-column-count)
                       :item-len item-len short-line)
                     (setf state :need-line)))))

                (:process-line
                 (trc nil "process" memory-no start)
                 (unless (< memory-no start)
                   (incf items-processed)
                   (delim-fields-collect-into item-fields header-info item
                     :end item-len
                     :parse-known-tags? parse-known-tags?)
                   (trc nil  "doing tab ct" item-tab-ct item-fields)
                   (funcall map-fn item item-len header-info item-fields))
                 (setf state :need-line))
            (values items-processed header-info)))))))


;;: delim-fields-collect-into
(defun delim-fields-collect-into (item-fields header-info item
                                   &key (end (length item))(parse-known-tags? t))
 (assert (<= end (length item)))
  (loop for col-no below (length item-fields)
      do (setf (aref item-fields col-no) nil))

  (let (salvaged-tag-pairs bogus)
    (declare (ignorable bogus))
    (flet ((do-column (col-no hdr-id value)
             
             
             ;
             ; note that we store a nil even if blank because the count of fields
             ; in the hash table tells us if we have a full record or if an 
             ; extraneous CR as stopped read-line before we got to the true end of record
             ;
             
             (setf (aref item-fields col-no)
               (unless (or (string= value "")(string= value "NIL"))
                 (If (and parse-known-tags? (find hdr-id *pq-tags-symbolic*))
                     (loop for itag-value in (tag-values-extract hdr-id value)
                         if (find #\, itag-value) do
                           (log-value-incf 'itag-with-comma-char)
                           ;(break "i-comma ~a ~a" hdr-id itag-value)
                           (loop for sub-value in (split-sequence #\, itag-value)
                               if (find #\= sub-value) do
                                 (log-value-incf 'itag-with-equal-char)
                                 (bif (tag-values (tag-pair-parse sub-value nil))
                                   (progn
                                     ;(trc "got comma and = in itags ~a" (cons hdr-id sub-value))
                                     ;(trc "tag pair would be" tag-values)
                                     (setf salvaged-tag-pairs (nconc salvaged-tag-pairs tag-values)))
                                   (setf bogus (cons hdr-id sub-value)))
                                     
                                   else do (push (list hdr-id sub-value) salvaged-tag-pairs))
                             else if (find #\= itag-value) do
                               (bwhen (tag-values (tag-pair-parse itag-value nil))
                                 ;(trc "got = in itags ~a" (cons hdr-id itag-value))
                                 ;(trc "tag pair would be" tag-values)
                                 (setf salvaged-tag-pairs (nconc salvaged-tag-pairs tag-values)))
                               (log-value-incf 'itag-with-equal-char)
                             else collect itag-value)
                   value )))))
      (loop for col-no upfrom 0
          for header-id across (header-std-headers header-info)
          for value in (split-sequence #\tab item :end end)
          do
             (setf value (spreadsheet-value-clean value))
             (when (and *ESCAPE-SYMBOLS-DICTIONARY* value)
               (multiple-value-bind (dlist mod-col)
                                    (decode-string value *ESCAPE-SYMBOLS-DICTIONARY* :no-stats nil nil)
                 (declare (ignore dlist))
                 (when mod-col
                   (setq value mod-col) )))
             (if header-id
               (do-column col-no header-id value)
               (setf (aref item-fields col-no) value) )
          finally
            (loop for (id . values) in salvaged-tag-pairs do
                  (loop for v in values do
                        (pushnew v (aref item-fields (gethash id (header-column header-info)))
                          :test 'string-equal)))
            (when (and bogus (itmf? partqastatus) (string> (itmf partqastatus) "4"))
              (trc "got , and = in itags ~a" bogus)
              ;(trc "tags are" (itmf tags))
              ;(trc "qa status" (itmf partqastatus) :desc (itmf itdesc))
              (log-value-incf 'tag-pair-unsalvageable))))))


