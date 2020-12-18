#+IDD-pkg
(in-package :IDD)

#-(or IDD-pkg default-pkg)
(in-package :wordnet)

;;: Consts  ===========================================================================================================

;;: *EXTERNAL-FORMAT*			A variant from Dan
(defparameter *EXTERNAL-FORMAT*
#+(or allegro clozure cmu sbcl)
  :iso-8859-1
#+clisp
  charset:iso-8859-1
#+lispworks
  ':latin-1
#-(or allegro clisp clozure cmu lispworks sbcl)
  (error "Need to specify ~s for ~s"
         '*EXTERNAL-FORMAT*
         (lisp-implementation-version) ))


;;: Classes ===========================================================================================================

;;: translator
(defclass translator ()
  ((a-stream :type stream :initarg :stream :reader get-stream)
   (memory :initarg :memory :reader memory) ))

;;: translator-into-xml
;; Translation result example:
;; <xml>
;;   <rel type="3"  id="309"  pos="0.0"        confirm="1" arity="0" ids=""          >09_Volume</rel>
;;   <rel type="7"  id="8935" pos="1.0"        confirm="1" arity="1" ids="3762"      >ACL/PCL</rel>
;;   <rel type="13" id="4456" pos="0.11111111" confirm="1" arity="2" ids="3762,4331"/>
;; </xml>
(defclass translator-into-xml        (translator) () )

;;: translator-into-named-text
;; Translation result example (names of related instances and evidence):
;; CONICAL	1.0
;; RIGHT/LEFT ANGULATION	VIDEO_GASTROSCOPE	0.2
(defclass translator-into-named-text (translator)
  ((delimiter :initarg :delimiter :reader delimiter))
  (:default-initargs :delimiter #\tab) )

;;: translator-into-text
(defclass translator-into-text       (translator)
  ((delimiter :initarg :delimiter :reader delimiter)
   (sub-delimiter :initarg :sub-delimiter :reader sub-delimiter) )
  (:default-initargs :delimiter #\; :sub-delimiter #\,) )


;;: CTORs =============================================================================================================

;;: make-translator-into-xml
(defun make-translator-into-xml        (a-stream &key mem) (make-instance 'translator-into-xml        :stream a-stream :memory mem))

;;: make-translator-into-text
(defun make-translator-into-text       (a-stream &key mem) (make-instance 'translator-into-text       :stream a-stream :memory mem))

;;: make-translator-into-named-text
(defun make-translator-into-named-text (a-stream &key mem) (make-instance 'translator-into-named-text :stream a-stream :memory mem))



;;: Methods ===========================================================================================================

;;: translator::put-header			Places a header in the stream.
(defmethod put-header ((trn translator)) nil)
(defmethod put-header ((trn translator-into-xml)) (write-line "<xml>"  (get-stream trn)))


;;: translator::put-footer			Places a footer in the stream.
(defmethod put-footer ((trn translator))          (terpri              (get-stream trn)))
(defmethod put-footer ((trn translator-into-xml)) (write-line "</xml>" (get-stream trn)))



;;: FUNCs =============================================================================================================


;;: join-strings
(defun join-strings (lst delimiter)
#+ALLEGRO
  (excl:list-to-delimited-string lst delimiter)
#-ALLEGRO
  (with-output-to-string (stream)
    (when (first lst)
      (write-string (first lst) stream) )
    (dolist (itm (rest lst))
      (write-string delimiter stream)
      (when itm
        (write-string itm stream) ))))


;;: process-file-by-lines
;;	mapping-function	a designator for a function of two arguments: a line (string) and the line length (the line doesn't contains EOL).
;;				If the function returns nil the process will be interupted.
;;	commit-function 	a designator for a function of one argument: the number of lines processed.
;;				The function is called after processing every STEP lines.
;; MARK: The following function is not portable since it uses the Franz specific read-line-into
;; function that avoids creating many new string objects. Replace by standard read-line
;; if you need portability.
(defun process-file-by-lines (file-name mapping-function &key step bypass limit commit-function (buffer-size 1000) (external-format :default))
  (let ((buffer (make-string buffer-size :initial-element #\null ))
        rv
        (i 0))
    (with-open-file (f file-name :direction :input :external-format external-format)
      (loop
        (fill buffer #\null)
        (setq rv (excl:read-line-into buffer f nil f :start 0))  ; Franz specific extention: non portable
        (if (eq rv f) (return))
        (incf i)
        (unless (and bypass (<= i bypass))
          (when (and (plusp rv) (char= (char buffer (1- rv)) #\Return))	;; Windows-style end-of-line on Unix box.
            (decf rv)
            (setf (char buffer rv) #\Null) )
          (unless (funcall mapping-function buffer rv)
            (return-from process-file-by-lines) )
          (when (and step (plusp step) (zerop (mod i step))) 
            (if commit-function (funcall commit-function i) (format t "~&lines: ~A" i)) )
          (when (and limit (>= i limit)) (return)) )))
    i ))



;;: MACROs ============================================================================================================

;;: with-open-files-ex
;;	file-defs	list of sublists. Each sublist is a parameter list of with-open-file.
;;			One extra parameter can be added to sublist: :optional t.
;;			Ii this case if filespec is bound to nil then stream will be bound to nil.
;; An example:
;;	(with-open-files-ex ((a anm) (b bnm) (c (f cnm) :optional t)) xxx)
(defmacro with-open-files-ex (file-defs &body body)
  (let (llet open-calls close-calls)
    (dolist (file-def file-defs)
      (destructuring-bind (stream filespec &key optional
                                                (direction         nil dir)
                                                (element-type      nil elm)
                                                (if-exists         nil if-e)
                                                (if-does-not-exist nil if-d)
                                                (external-format   nil ext) )
                          file-def
        (let ((fle-spc filespec)
              open-call prms )
          (when (and optional (not (symbolp filespec)))
            (setq fle-spc (gensym))
            (push `(,fle-spc ,filespec) llet) )
          (push stream llet)
          (setq prms (list fle-spc))
          (when dir  (nconc prms (list :direction         direction)))
          (when elm  (nconc prms (list :element-type      element-type)))
          (when if-e (nconc prms (list :if-exists         if-exists)))
          (when if-d (nconc prms (list :if-does-not-exist if-does-not-exist)))
          (when ext  (nconc prms (list :external-format   external-format)))
          (setq open-call `(open ,@prms))
          (when optional
            (setq open-call `(when ,fle-spc ,open-call)) )
          (push `(setq ,stream ,open-call)      open-calls)
          (push `(when ,stream (close ,stream)) close-calls) )))
   `(let ,(nreverse llet)
      (unwind-protect
        (progn ,@(nreverse open-calls) ,@body)
        (progn ,@close-calls) ))))
