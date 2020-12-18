#+IDD-pkg
(in-package :IDD)
#-(or IDD-pkg default-pkg)
(in-package :wordnet)


;;: check-COLUMN-IDXS
(defun check-COLUMN-IDXS (idxs)
  (let ((last-idx -1)
        vidxs )
    (do ((idx idxs (cddr idx)))
        ((null idx))
      (unless (cdr idx)
        (error "check-COLUMN-IDXS: Odd length of column indexes list: ~S" idxs) )
      (unless (and (fixnump (first idx)) (>= (first idx) 0))
        (error "check-COLUMN-IDXS: Invalid column index ~S" (first idx)) )
      (when (= (first idx) last-idx)
        (error "check-COLUMN-IDXS: Duplicated column index ~S" (first idx)) )
      (unless (> (first idx) last-idx)
        (error "check-COLUMN-IDXS: Invalid order of column indexes: ~S" idxs) )
      (unless (and (fixnump (second idx)) (>= (second idx) 0))
        (error "check-COLUMN-IDXS: Invalid vector index ~S" (second idx)) )
      (when (position (second idx) vidxs)
        (error "check-COLUMN-IDXS: Duplicated vector index ~S" (second idx)) )
      (setf last-idx (first idx))
      (push (second idx) vidxs) )))


;;: strip-quotes
(defun strip-quotes (txt)
  (let ((l (length txt)))
    (if (and (> l 1) (char= (char txt 0) #\"
                                              ))
      (subseq txt 1
        (if (char= (char txt (1- l)) #\"
                                        )
	  (1- l)
	  l ))
       txt )))



;;: trim-parse-smart-upcase
;; Returns:
;;	list (simple-string* )
(defun trim-parse-smart-upcase (column-string delim-setup blacklist)
  (when (and column-string (plusp (length column-string)))
    (nstring-upcase column-string)
    (nset-difference
      (delete-if (lambda (x) (position #\= x))
        (if (first delim-setup)
          (delete-duplicates 			;; multiple-valued
            (tokenizer-by-deliminators column-string (first delim-setup) :discard      '(#\Space)
                                                                         :rtrim        '(#\.)
                                                                         :strip-quotes nil ))
          (when (plusp (length column-string))	;; single-valued
            (list column-string) )))
      blacklist
      :test #'string= )))



;;: tokenizer-by-deliminators
;;	delimiters	list of char.
;;	table		nil or t or list (src-column-index+) or list ({src-column-index dest-vector-index}+)
;;			src-column-index must be listed in ascending order.
;;			If table then pairs, suffixes, prefixes, conjunctions must be nil.
;;	dest-vector	vector of strings. If dest-vector is present table must contain dest-vector-index values.
;;	pairs
;;	positions	ignored if pairs, or not omit-delimiters, or dest-vector.
;;	omit-delimiters
;;	suffixes
;;	prefixes
;;	conjunctions
;; Returns:
;;	dest-vector if dest-vector is present.
;;	or list of strings if dest-vector is omitted and table is not a list.
;;	or list of sublists (src-column-index string) if dest-vector is omitted and table is a list.
;;	or nil if dest-vector is not omitted.
;; Examples:
;;	(tokenizer-by-deliminators "||two|three||five|" '(#\|))
;;	 => ("two" "three" "five")
;;	(tokenizer-by-deliminators "||.two.|th.ree|.|five.|" '(#\|) :rtrim '(#\.))
;;	 => (".two" "th.ree" "five")
;;	(tokenizer-by-deliminators "||two|three||five" '(#\|) :pairs t)
;;	 => ("two" "three" "five")
;;	    ("|" "||" NIL)
;;	(tokenizer-by-deliminators "||two|three||five" '(#\|) :positions t)
;;	 => ("two" "three" "five")
;;	    (2 6 13)
;;	(tokenizer-by-deliminators "||two|three||five" '(#\|) :omit-delimiters nil)
;;	 => ("||" "two" "|" "three" "||" "five")
;;	(tokenizer-by-deliminators "||two|three||five" '(#\|) :pairs t :omit-delimiters nil)
;;	 => (("two" . "|") ("three" . "||") ("five"))
;;	(tokenizer-by-deliminators "||two|three||five|" '(#\|) :table t)
;;	 => ("" "" "two" "three" "" "five" "")
;;	(tokenizer-by-deliminators "||two|three||five|" '(#\|) :table '(1 3 5 20))
;;	 => ((1 "") (3 "three") (5 "five"))
;;	(setq vec '#("old" "old" "old" "old" "old"))
;;	(tokenizer-by-deliminators "||two|three||five|" '(#\|) :table '(1 3  3 2  5 1  20 0) :dest-vector vec)
;;	 => nil
;;	vec => #("old" "five" "three" "" "old")
;;	(tokenizer-by-deliminators "%12% 13%ABC A&B AA& BB AAA &BBB AAA & BBB 16%" '(#\Space) :suffixes '(#\%) :conjunctions '(#\&))
;;	 => ("%" "12%" "13%" "ABC" "A" "&" "B" "AA" "&" "BB" "AAA" "&" "BBB" "AAA" "&" "BBB" "16%")
;;	(tokenizer-by-deliminators "&X A&B #ZZ AA#BB&" '(#\Space) :suffixes '(#\%) :prefixes '(#\#) :conjunctions '(#\&))
;;	 => ("&" "X" "A" "&" "B" "#ZZ" "AA" "#BB" "&")
(defun tokenizer-by-deliminators (str 
                                  delimiters
                                  &key 
                                  (discard    '(#\Tab #\Null #\Space #\Newline))
                                  (omit-delimiters t)
                                  (strip-quotes t)
                                  rtrim
                                  pairs
                                  positions
                                  table
                                  dest-vector
                                  suffixes
                                  prefixes
                                  conjunctions
                                  confirmf )
  (declare (simple-string str))
  (declare (list delimiters))
#+DEBUG
  (assert (or (not table) (and (not suffixes) (not conjunctions) (not prefixes))))
  (let ((start 0)
        (end (or (position #\Null str) (length str)))
        (extract (when (listp table) table))
        begin-of-token end-of-token token )
    (labels
          ((get-token ()
                      (when (or table (< (the fixnum start) (the fixnum end)))
                        (loop for end-of-token upfrom start below end do
                          (let ((c (char str (the fixnum end-of-token))))
                            (declare (character c))
                            (when (and (position c delimiters)	;; end of a token
                                       (or (not confirmf)
                                           (funcall confirmf c str start end-of-token) ))
                              (when table
                                (return-from get-token (values start end-of-token (1+ (the fixnum end-of-token)))) )
                              (let ((end-of-delim (1+ (the fixnum end-of-token))))
                                (declare (fixnum end-of-delim))
                                (loop while (and (< end-of-delim (the fixnum end))
                                                 (position (setq c (char str end-of-delim)) delimiters)
                                                 (or (not confirmf)
                                                   (funcall confirmf c str start end-of-delim) ))
                                  do (incf end-of-delim) )
                                (return-from get-token (values start end-of-token end-of-delim)) ))
                            (when (and suffixes (position c (the list suffixes)))
                              (incf (the fixnum end-of-token))
                              (when (>= (the fixnum end-of-token) (the fixnum end))
                                (return-from get-token (values start end-of-token #|nil|#)))
                              (let ((end-of-delim end-of-token))
                                (declare (fixnum end-of-delim))
                                (loop while (and (< end-of-delim (the fixnum end))
                                                 (position (setq c (char str end-of-delim)) delimiters)
                                                 (or (not confirmf)
                                                     (funcall confirmf c str start end-of-delim) ))
                                  do (incf end-of-delim) )
                                (return-from get-token (values start end-of-token end-of-delim)) ))
                            (when (and prefixes (> (the fixnum end-of-token) (the fixnum start)) (position c (the list prefixes)))
                              (return-from get-token
                                (values start end-of-token end-of-token) ))
                            (when (and conjunctions (position c (the list conjunctions)))
                              (when (= (the fixnum end-of-token) (the fixnum start))
                                (incf (the fixnum end-of-token))
                                (when (>= (the fixnum end-of-token) (the fixnum end))
                                  (return-from get-token (values start end-of-token #|nil|#)))
                                (let ((end-of-delim end-of-token))
                                  (declare (fixnum end-of-delim))
                                  (loop while (and (< end-of-delim (the fixnum end))
                                                   (position (setq c (char str end-of-delim)) delimiters)
                                                   (or (not confirmf)
                                                       (funcall confirmf c str start end-of-delim) ))
                                    do (incf end-of-delim) )
                                  (return-from get-token (values start end-of-token end-of-delim)) ))
                              (return-from get-token (values start end-of-token end-of-token)) )))
                        (values start end #|nil|#) ))
           (r-trim ()
                   (loop while (and (< begin-of-token end-of-token)
                                    (position (char str (1- end-of-token)) rtrim) )
                     do (decf end-of-token) ))
           (strip-q ()
                    (if (and (> (- end-of-token begin-of-token) 1)
                             (char= #\"
                                   (char str begin-of-token)
                                   (char str (1- end-of-token)) ))
                      (progn
                        (decf end-of-token)
                        (incf begin-of-token)
                        (when rtrim (r-trim))
                        (setq token (subseq str begin-of-token end-of-token))
                        (let ((i (position #\"
                                           token )))
                          (when i
                            (loop while i do
                              (unless (char= #\"
                                            (char token (incf i)) )
                                (return) )				;; break loop
                              (setq i (position #\"
                                                (replace token '#(#\Null) :start1 i)
                                                :start i )))
                            (delete #\Null token) )))
                      (progn
                        (when rtrim (r-trim))
                        (setq token (subseq str begin-of-token end-of-token)) ))))
      (let (tokens dlmtrs delimiter places begin-of-dlmtr)
        (loop
          for i upfrom 0 do
          (multiple-value-setq (begin-of-token end-of-token start) (get-token))
          (setq begin-of-dlmtr end-of-token)
          (unless begin-of-token (return))
          (if extract				;; table is list
            (progn
              (when (= i (first extract))
                (if strip-quotes
                  (strip-q)
                  (progn
                    (when rtrim (r-trim))
                    (setq token (subseq str begin-of-token end-of-token)) ))
                (when discard (setq token (string-trim discard token)))
                (if dest-vector
                  (progn
                    (setf (aref dest-vector (second extract)) token)
                    (setq extract (cddr extract)) )
                  (progn
                    (push (list i token) tokens)
                    (setq extract (rest extract)) )))
              (unless (and extract start) (return)) )
            (progn
              (if strip-quotes
                (strip-q)
                (progn
                  (when rtrim (r-trim))
                  (setq token (subseq str begin-of-token end-of-token)) ))
              (when discard (setq token (string-trim discard token)))
              (when (or table (> (length token) 0))
                (if pairs
                  (progn
                    (if start
                      (progn
                        (setq delimiter (subseq str begin-of-dlmtr start))
                        (when discard (setq delimiter (string-trim discard delimiter)))
                        (when (zerop (length delimiter))
                          (setq delimiter nil) ))
                      (setq delimiter nil) )
                    (if omit-delimiters 
                      (progn
                        (push token tokens)
                        (push delimiter dlmtrs) )
                      (push (cons token delimiter) tokens) ))
                  (progn
                    (push token tokens)
                    (when positions
                      (push begin-of-token places) ))))
              (unless start (return))		;; break the loop
              (unless (or omit-delimiters pairs)
                (setq delimiter (subseq str begin-of-dlmtr start))
                (when discard (setq delimiter (string-trim discard delimiter)))
                (when (or table (> (length delimiter) 0)) (push delimiter tokens)) ))))
        (or dest-vector
            (if positions
              (values (nreverse tokens) (nreverse places))
              (values (nreverse tokens) (nreverse dlmtrs)) ))))))



;;: extract-key-param
;;	keys	list of keys.
;;	prms	list of pairs ({key value}*).
;; Returns:
;;	prms-reduced	list. prms without extracted pairs.
;;	key-values	list of values of the keys
;; An example:
;;	(extract-key-param '(:IF-NEW :IF-OLD) '(:name "X" :IF-NEW '(incf var)))
;;	=>
;;	(:name "X")
;;	((incf var) nil)
(defun extract-key-param (keys prms)
  (let ((key-values (make-list (length keys)))
        prms-reduced i )
    (do ((rm prms (cddr rm)))
        ((null (cdr rm)) (when rm (error "Length of ~S is odd" prms)))
      (if (setq i (position (first rm) keys))
        (setf (nth i key-values) (second rm))
        (progn
          (push (first  rm) prms-reduced)
          (push (second rm) prms-reduced) )))
    (values (nreverse prms-reduced) key-values) ))



;;: extract-key-param-ex
;;	keys	list of keys.
;;	prms	list of pairs ({key value}*).
;; Returns:
;;	prms-reduced	list. prms without extracted pairs.
;;	prms-ex		list. Extracted pairs with values substituted by variable names from let-list.
;;	let-list	list of sublists for let.
;;	key-values	list of values of the keys.
;; An example:
;;	(extract-key-param-ex '(:evidence :name :confirmation) '(:FEATURETYPE (id rel) :evidence (* tc tf) :confirmation 1))
;;	=>
;;	(:FEATURETYPE (id rel))
;;	(:evidence #G:1000 :confirmation 1)
;;	((#G:1000 (* tc tf)))
;;	(#G:1000 nil 1)		;; the values of evidence, name and confirmation.
(defun extract-key-param-ex (keys prms)
  (let (prms-ex let-list)
    (multiple-value-bind (prms-reduced key-values) (extract-key-param keys prms)
      (do ((v key-values (rest v)) (k keys (rest k))) ((null v))
        (let ((p (first v)))
          (when p
            (when (listp p)
              (push (list (gensym) p) let-list)
              (setq p (caar let-list))
              (setf (first v) p) )
            (push (first k) prms-ex)
            (push p         prms-ex) )))
      (values prms-reduced (nreverse prms-ex) (nreverse let-list) key-values) )))


