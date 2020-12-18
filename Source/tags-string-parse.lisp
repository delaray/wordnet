(in-package :wordnet)

(defun string-ok-as-tag-id? (s)
  ;;(trc "ok?" s)
  (unless (or (string= s "") (find #\space s))
    (search s "1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 n t N T")))

(defun log-value-incf (value-name &optional (delta 1) (do-it t))
  (when (and do-it (boundp '*log*) *log*)
    (if (consp value-name)
        (destructuring-bind (alist-name entry-name) value-name
          (let* ((alist (slot-value *log* alist-name)))
            (bif (entry (assoc entry-name alist))
              (incf (cdr entry) delta)
              (push (cons entry-name delta) alist))
            (setf (slot-value *log* alist-name) alist)
            #+itscool (break "cool? ~a ~a" value-name alist)
            ))
      (setf (slot-value *log* value-name)
        (+ delta (or (slot-value *log* value-name) 0))))))

#+test
(tag-pair-parse "=BROWBROWBROW GUARD==BROWBROWBROW GUARD=BROWBROWBROW GUARDBROWBROWBROW GUARD" nil)

#+test
(tag-pair-parse  "12=INFLOW; NONROTATING; VERTICAL STOPCOCKS; 20=A2642T" nil)

#+test
(tag-string-parse "N=CASE, T=STERILIZATION, 1=ZPLATE; ATL, 11=ANTERIOR, 12=BOLT, SCREW, AND PLATE")

#+test
(TAG-PAIR-PARSE "1=DOSE =___MG =___MLS" 'TYPE)

(defun tag-pair-parse (tps prior-tag-id &aux (tag-pair-string (string-trim "; " tps)))
  (when (empty$ tag-pair-string)
    (return-from tag-pair-parse nil))

  ;; sorry 'bout this hardcode solution to Focal length madness in tags
  (bwhen (fpos (search "F=" tag-pair-string))
    (setf (schar tag-pair-string (1+ fpos)) #\space))

  (let* ((tag-pair-parts (ukt:split-sequence #\= tag-pair-string))
         (tpplen (count-if-not 'empty$ tag-pair-parts)))
    (cond
     ((= tpplen 1) ;; no tag or no value
      (let ((pos (position-if-not 'empty$ tag-pair-parts)))
        (cond
         ((zerop pos) (if (string-ok-as-tag-id? (car tag-pair-parts))
                          (progn
                            (log-value-incf 'TAG-PAIR-ID-ONLY)
                            (list (cons (string-to-tag (car tag-pair-parts)) nil)))
                        (let ((this-tag-id (case prior-tag-id
                                             ((nil) 'noun) ;; hail mary, but we saw "SET,T=SPINAL, etc"
                                             (noun 'type)
                                             (otherwise prior-tag-id))))
                          (log-value-incf 'TAG-PAIR-ID-MISSING)
                          (trc nil "applying prior tag id!!!! ~a to get ~a for ~a" prior-tag-id this-tag-id tps)
                          
                          (list (list this-tag-id (find-if-not 'empty$ tag-pair-parts))))))
         ((= 1 pos) (let ((this-tag-id (case prior-tag-id
; ?????                                  ((nil) (break "untagged value ~a first" tps))
                                         ((nil) (return-from tag-pair-parse nil))
                                         (noun 'type)
                                         (otherwise prior-tag-id))))
                      (log-value-incf 'TAG-PAIR-ID-MISSING)
                      (trc nil "also applying prior tag id!!!! ~a to get ~a for ~a" prior-tag-id this-tag-id tps)
                      (list (list this-tag-id (find-if-not 'empty$ tag-pair-parts)))))
         (t (log-value-incf 'TAG-PAIR-UNSALVAGEABLE)
           (trc "very sick tag-pair ~a" tps)))))
       
     ((= tpplen 2)
      (destructuring-bind (id value)
          (remove-if 'empty$ tag-pair-parts)
        (bwhen (pq-id (if (string-ok-as-tag-id? id)
                          (string-to-tag id)
                        (progn
                          (trc nil "ignoring another sick tag-pair ~a" tps)
                          (log-value-incf 'TAG-PAIR-UNSALVAGEABLE)
                          nil )))
          (list (cons pq-id (tag-values-extract pq-id value))))))

     (t 
      ; #+justtossit!!! ;; we are only trying to augment itags
     (let* ((last-delim (position #\= tag-pair-string :from-end t))
               (end-bad-pair (position-if-not 'alphanumericp tag-pair-string
                               :from-end t
                               :end last-delim)))
          (if (end-bad-pair-works end-bad-pair tag-pair-string last-delim)
              (let ((brk-at (1+ end-bad-pair)))
                (log-value-incf 'TAG-PAIR-UNJOINED-OK)
                (nconc (tag-pair-parse (let ((noblanks (string-trim " " (subseq tag-pair-string 0 brk-at))))
                                         (if (find (schar noblanks (1- (length noblanks))) ".;")
                                             (subseq noblanks 0 (1- (length noblanks)))
                                           noblanks)) prior-tag-id)
                  (tag-pair-parse (subseq tag-pair-string brk-at) prior-tag-id)))
            (let ((two (split-once tag-pair-string #\=)))
              (if (string-ok-as-tag-id? (car two))
                  (tag-pair-parse (substitute #\space #\= tag-pair-string :start (1+ (position #\= tag-pair-string))) prior-tag-id)
                (progn (log-value-incf 'TAG-PAIR-UNSALVAGEABLE)
                  nil)))))))))

#+test
(split-slash '|20| "so a/b" nil)

(defun split-slash (tag v semicolon-ever-used &aux (bafter 4)) ;;itdesc tag-string
  (declare (ignorable bafter semicolon-ever-used))
  (let ((*brka-mode* :silent))
    (if semicolon-ever-used
        (list v)
      (case tag
        ((noun type)) ;; slashes expected in noun or type
        (trademark (list (substitute #\space #\/ v)))
        (otherwise
         (unless (find #\space v)
           (bwhen (sl-pos (position #\/ v))
             (unless (or (zerop sl-pos)
                       (>= sl-pos (1- (length v)))
                       (or (digit-char-p (schar v (1- sl-pos)))
                         (digit-char-p (schar v (1+ sl-pos)))))
               (let ((vs (delete-if (lambda (s) (string= "" s))
                           (loop for seg in (split-sequence #\/ (string-trim " "  v))
                               collecting  (string-trim " " seg)))))
                 
                 ;(incf (cdr (assoc 'split *ss*)))
                 (break-after (*brka* 0) "SPLIT!!! ~a into ~a from ~a" tag vs v)
                 (unless (every (lambda (v)
                                  (parse-integer v :junk-allowed t))
                           vs)
                   (when *log* (incf (slash-treated-as-semicolon *log*) (1- (length vs))))
                   vs))))))))))

(defun tag-values-extract (tag-id value)
    ;
    ; first separate possible intended multiples with prescribed ;
    ; then look to see if they used / by mistake, then look for extraneous chars
    ;
  (loop for tv in (let* ((segs (split-sequence #\; value))
                         (any-semicolon (cdr segs)))
                    (loop for v in (loop for s in segs
                                         for s-clean = (string-trim " =;," s)
                                         unless (empty$ s-clean)
                                         collect s-clean)
                        nconcing (or (split-slash tag-id v any-semicolon)
                                   (list v))))
      unless (empty$ tv)
      collect tv))                       

#+test
(string-trim " =" " =heme")
#+test
(let ((*log* nil))
  (tag-values-extract 'color "aaa/bbb/ccc"))

#+test
(tag-pair-parse "1=A/B")

#+test
(let* ((s "6=a=15.5 IN X W11 IN X H17.75 IN")
       (ebp (position-if-not 'alphanumericp s
              :from-end t
              :end (position #\= s :from-end t))))
  (end-bad-pair-works ebp s (position #\= s :from-end t)))

#+test
(tag-string-parse "a=1,b=2,c=3., 14=4; e=42")

#+test
(tags-split "N=SPLINT;T=ORTHOPEDIC;1=ALIGNRITE;2=COTTON; RUBBER FABRIC;12=WITHOUT WRAP AROUND STRAP
;14=LATEX FREE;5=XSMALL; SHORT 11=WRIST; LEFT")

#+TEST
(tags-split "N=CASE, T=STERILIZATION, 1=ZPLATE; ATL, 11=ANTERIOR, 12=BOLT, SCREW, AND PLATE")


(defun un-conjunct (s)
  (bif (or-pos (search "OR " s))
    (loop for c across s
          for n below or-pos
          unless (spacep c) do (return)
          finally  (setf s (subseq s (+ or-pos 3))))
    (bif (and-pos (search "AND " s))
      (loop for c across s
          for n below and-pos
          unless (spacep c) do (return)
          finally  (setf s (subseq s (+ and-pos 4))))))
  s)

(defun tags-split (tags-string)
  (let* ((approx-tag-ct (1- (count #\= tags-string))) ;; sometimes get extra = in value
         (raw-split (mapcar 'trim$
                      (let ((take-1 (ukt:split-sequence *tag-separator-in-use* tags-string)))
                        (if (>= (length take-1) approx-tag-ct)
                            take-1
                          (let* ((take-2-delim (case *tag-separator-in-use*
                                                 (#\, #\;)(#\;
						               #\,)))
                                 (take-2 (ukt:split-sequence take-2-delim tags-string)))
                            (flet ((use-take-2 ()
                                     (trc "switching to alternate delimiter" take-2-delim)
                                     (setf *tag-separator-in-use* take-2-delim)
                                     take-2))
                              (if (> (length take-2) approx-tag-ct)
                                  (use-take-2)
                                (if (> (length take-2)(length take-1))
                                    (use-take-2) take-1)))))))))
    ;(trcx raw-split)
    (loop with building-n
        for tag-n upfrom 0
        for raw-pair in raw-split
        for eq-pos = (position #\= raw-pair)          
        if (null eq-pos) do
          (if (null building-n)
              (trc nil "nowhere to append continuation ~a overall ~a"
                raw-pair tags-string)
            (progn
              (setf (nth building-n raw-split) (conc$ (nth building-n raw-split) "; "
                                                 (un-conjunct raw-pair)))
              (setf (nth tag-n raw-split) nil)))
        else if (not (string-ok-as-tag-id? (subseq raw-pair 0 eq-pos))) do
          (loop for id-size downfrom 2 to 0
              if (zerop id-size) do
                (trc nil "Cannot repair tag-pair ~a :in-tag-string ~a" raw-pair tags-string)
                (setf (nth tag-n raw-split) nil)
              else if (string-ok-as-tag-id? (subseq raw-pair (- eq-pos id-size) eq-pos)) do
                (when building-n
                  ;; give trimmed prefix to prior tag if there is one
                  (setf (nth building-n raw-split)
                    (conc$ (nth building-n raw-split) "; " (subseq raw-pair 0 (- eq-pos id-size)))))
                (setf (nth tag-n raw-split) (subseq raw-pair (- eq-pos id-size)))
                (loop-finish))
        else do
          (setf building-n tag-n)
        finally (return (delete nil raw-split)))
    ))
      
#+test
(tags-split
 "NN=CAST, T=ORTHOPEDIC, 1=SCOTCHCAST PLUS, 2=FIBERGLASS, 6=L4 YD X W2 IN, 12=TAPE; LIGHT WEIGHT, 14=LATEX FREE, 16=BRIGHT ORANGE")

#+test
(tag-string-parse "N=OBJECTIVE, T=MICROSCOPE, 12=FOCAL DISTANCE F=150 MM; RED REFLEX; USED")

#+test
(tag-string-parse  "N=NIF, T=, 20=INSUFFICIENT INFORMATION FOUND IN CATALOG/INTERNET")

#+test
(tag-string-parse "SET, T=SPINAL, 1=UNIVERSAL, 2=TITANIUM, 12=HOOK/ROD/CLAMP INSTRUMENT SYSTEM")

#+test
(tag-string-parse "SET, T=SPINAL, 1=UNIVERSAL, 12=ROD INSTRUMENT SYSTEM")

#+test
(tag-string-parse "N=LABEL, T=MEDICAL, 1=DOSE =___MG =___MLS, 6=L2 1/4 IN")

#+test
(tag-string-parse "N=NEEDLE, T=SCLEROTHERAPY, 6=L230 MM, 7=L4 MM, 8=OD25 GA; ODSEC2.8 MM, 11=ESOPHAGEAL VARICES 12=INJECTOR; CHANNEL; SHEATH; ENDOSCOPIC, 15=DISPOSABLE")

(defun tag-string-parse (tags-string)
  (loop with prior-tag-id
      for tag-pair in (tags-split tags-string)
      for tpp = (tag-pair-parse tag-pair prior-tag-id)
      nconc (progn (assert (listp tpp)() "tpp atom (not list) val ~a from ~a prior ~a"
                     tpp tag-pair prior-tag-id)
              ;(trcx tpp)
              (when (caar tpp)
                (setf prior-tag-id (caar tpp)))
              tpp)))

#+test
(tags-split "N=GOGGLES, T=PROTECTIVE, 1=UVEX/ASTROSPEC 3000, 2=POLYCARBONATE; NYLON, 6=W135 MM, 12=GLASSES; PROTECTIVE EYEWEAR IMPACT RESISTANT; SIDE SHIELDS BROW\
GUARD, =BROWBROWBROW GUARD==BROWBROWBROW GUARD=BROWBROWBROW GUARDBROWBROWBROW GUARD")

#+test
(tags-split
 "N=SEAL;T=SECURITY;1=SNUBBERS;12=PISTON CENTER JOINT;20=010;5=5000 PSIG; .25 IN NPT;6=L3.46 IN")

(defun split-tag-phrase (p)
  (nset-difference (split-seq p #\space)
    '("for" "and" "the" "products" "or") :test 'string-equal))

(defun split-once (s delim &aux (brk (position delim s)))
  (if brk
      (list (subseq s 0 brk)(subseq s (1+ brk)))
    (list s)))

(defun end-bad-pair-works (n s end)
  (when n
    (let ((tag (subseq s (1+ n) end)))
      (and
       (case (length tag)
         (0 nil)
         (1 t)
         (otherwise (every 'digit-char-p tag)))
       (notany (lambda (seg)
                 (zerop (length seg)))
         
         (ukt:split-sequence #\= (subseq s 0 (1+ n))))))))

(defun subst-word-for-char (s w c &aux (len (length s)))
  (loop for p = (position c s)
      while p
      do (trc nil "subst word for char" s w c)
        
        (count-it :subst-word-for-char)
        (when (and (boundp '*log*) *log*)
          (log-value-incf 'tag-chars-cleaned))
        (setf s (conc$ (subseq s 0 p)
                  (unless (or (zerop p) (eql #\space (schar s (1- p)))) " ")
                  w
                  (unless (and (< (1+ p) len)(eql #\space (schar s (1+ p)))) " ")
                  (subseq s (1+ p))))
        finally 
        (return s)))

(defun subst-word-for-word (s new old &aux (new-len (length new)))
  (unless (and new (string= new old))
    (loop with start2 = 0
        for p = (search old s :start2 start2)
        while p
        do (setf s (conc$ (subseq s 0 p)
                     new
                     (subseq s (+ p (length old))))
             start2 (+ p new-len))
        finally (return s))))

#+test
(subst-word-for-word "BIG-AND-BLUE" "YABBA" "AND")

#+test
(loop for w in '("P&L" "J & J" "J& J" "J &J" "&J" "J&")
      collect (cons w (subst-word-for-char w "AND" #\&)))

(defun subst-or-for-/ (s &aux (len (length s)))
  (loop repeat 3
      for p = (position #\/ s) then (position #\/ s :start (1+ p))
      while p
        unless (or (and (plusp p) (digit-char-p (schar s (1- p))))
                 (and (< (1+ p) len)(digit-char-p (schar s (1+ p)))))
      do (when (and (boundp '*log*) *log*) (incf (tag-chars-cleaned *log*)))
        (setf s (conc$ (subseq s 0 p)
                  (unless (or (zerop p) (eql #\space (schar s (1- p)))) " ")
                  "OR"
                  (unless (and (< (1+ p) len)(eql #\space (schar s (1+ p)))) " ")
                  (subseq s (1+ p))))
      finally (return s)))

#+test
(loop for w in '("1/2" "Level 1/2" "TYPE A/B" "Level 1 / 2")
    collect (cons w (subst-or-for-/ w)))

#+test
(loop with prior-tag-id
    for tag-pair in (tags-split "=MAX; N=CASE, T=STERILIZATION, 1=ZPLATE; ATL, 11=ANTERIOR, 12=BOLT, SCREW, AND PLATE")
    for tpp = (tag-pair-parse tag-pair prior-tag-id)
    nconc (list prior-tag-id)
    nconc (progn (assert (listp tpp)() "tpp one val ~a from ~a prior ~a"
                   tpp tag-pair prior-tag-id)
            (when (caar tpp) (setf prior-tag-id (caar tpp)))
            (when (cadar tpp) tpp)))

