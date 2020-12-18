;;;(eval-now! (proclaim '(optimize (speed 2) (safety 1) (space 1) (debug 3))))

#+ugh
(progn
  (setf (sys:gsgc-parameter :generation-spread) 1)
  ;(excl:gc :tenure)
  ;(excl:gc t)
  (sys:resize-areas :new (* 400 1024 1024) :old (* 400 1024 1024)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :acache 
           #-allegro-v8.1 "acache-2.1.4.fasl"
           #+allegro-v8.1 "acache-2.1.8.fasl"))

#+nahhh
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :agraph))

#+test
(triple-index-flavors)

;; 02/04/08 efalkin:  changing from package :cleaner to package :wordnet
;; 11/19/07 mrh:  changing from package :pq to package :cleaner
;; mrh: can't import from PQ, because this will cause same name conflict we had before.

(defpackage :wordnet
  (:use :common-lisp :excl :utils-kt :db.ac :db.ac.utils #+notyet :triple-store)
  (:export 
   #:ghx-reference-build
   #:find-word-definitions
   #:find-path-between-words
   #:word-in-dictionary-p
   #:set-cleaner-initial-resource-directory
   #:set-cleaner-data-directory
   #:make-cleaner-resources-pathname

   #:itmcol #:itmcol* #:itmf #:itmf? #:chg-f #:chg-f* #:item-col* #:itm-f #:itm-f?
   #:upcase? #:singularify-ex #:vendor #:brand-name #:nt-masters-get #:commodity
   #:taxonomy-node #:subst-word-for-word #:subst-word-for-char #:mistaggings-avoided
   #:noun-type-reversal-items #:pq-tag-symbol-to-encoded-string #:items-fixed
   #:item-fields-to-tag-string #:header-std-headers #:header-column #:header-source-headers
   #:items-read #:itdesc-to-tags #:log-end-time
   #:unabbreviate #:abbreviate #:word-net-lookup #:formatting-fixes #:spelling-fixes
   #:split-tag-phrase #:singularify #:attributes-retagged
   #:master-type-p #:master-noun-p
   #:ghx-reference-open-ensure #:cleaning-log-open-ensure #:word-net-reset
   #:invalid-taxonomy-node #:taxo-minus-1 #:find-tag-map #:tag-maps-list #:map-delimited
   #:ensure-map #:recreate-tag-map #:log-value-incf #:make-log-entry #:process-file-by-lines
   #:cleaning-log-create
   #:distinct-tag-words #:distinct-noun-types #:make-noun-type-key #:tags-itags-combine
   #:trivial-distinct-noun-types-to-reverse #:distinct-noun-types-to-reverse
   #:get-nt-type #:get-nt-noun #:with-log-db

   SPEC-STRING
   #:SPST-sort-key #:SPST-sort-key2 #:SPST-sort-key3 #:SPST-key
   #:SPST-value #:SPST-sample #:SPST-comment #:SPST-files #:SPST-count
   #:read-convert-dict #:html-encode

   *cleaner-initial-resource-directory*
   *cleaner-data-directory*
   *cleaning-log* *pq-tags-symbolic* *measure-tags* *log* *RAW* *tag-separator-in-use*
   *pq-tag-symbols-in-order* *UNSPSC-OBSOLETE*
   *ESCAPE-SYMBOLS-DICTIONARY*
   *EXTERNAL-FORMAT*
   item-fields header-info change-reasons
   .noun .type .tags .itdesc .taxonomynode .venname .normvenname .vencatnum .trademark
   tag-map tag-map-by-word
	   ))
;
(in-package :wordnet)

(defparameter *raw* nil)

(defparameter *cleaner-initial-resource-directory* nil)
;;;  #+mswindows
;;;  (make-pathname :directory '(:absolute "0ghx" "Cleaner" "initial-resources"))
;;;  #+unix
;;;  (make-pathname :directory '(:absolute "home" "kenny" "Cleaner" "initial-resources")))

(defun set-cleaner-initial-resource-directory (path)
  (assert (probe-file path) () "PQ Cleaner initial resources directory ~a does not exist.
Create it using the OS file manager and populate with WordNet 
and other initialization data and then try again." path)
  (setf *cleaner-initial-resource-directory* path))

(defparameter *cleaner-data-directory* nil)

(defun set-cleaner-data-directory (path)
  (assert (probe-file path) () "PQ Cleaner data root directory ~a does not exist.
Create it using the OS file manager then try again." path)
  (setf *cleaner-data-directory* path))

(defun make-cleaner-resources-pathname (path-to-extend)
  (merge-pathnames path-to-extend *cleaner-initial-resource-directory*))


#| Original design thoughts follow..........................


The Mission

-- misspellings
-- word order
-- missing values
-- wrong tag for value (size tag used for age)

So the plan is this:

Build "GHX Reference" DB
------------------------
Load word-net, master noun-type, anything else we can find that establishes facts 
independent of the data feed. Eventually encode here the tag specific must-be data 
(attribution rules) so it does not end up in the code.

Build "Raw Resource" DB
-----------------------
Read one or more reference files into a DB of maps of raw values with counts. Hmmm. Maybe
this should be enhanced eventually to age counts so new data more easily pushes out the old.

Build "Resource Erratum" DB
---------------------------
Chomp all over "Raw Resource" deciding respellings and tag changes, producing 
new maps from raw tag/data to better (possibly a respelling, possibly a tag move,
possibly both. Not sure about the latter. Write out only things that change, so a 
"not found" lookup means "yer good". Hence "erratum".

Clean a file
------------
Convert any given ref file by copying it, correcting it against the erattum DB.

Details on Cleaning Conversion
------------------------------
# Cleaning will involve each value of a tag and each word of an itdesc being looked up and
# either respelled and/or moved between tags, or added to a tag from the itdesc. trademarks
# will look in a dedicated map, others will look in alltags. breaking out itdesc will involve
# both alltags and trademark.

- Look up each tag value. If spelling change, go to that unless itdesc supports something
  else (eg. artrial -> atrial, arterial). If in wrong tag at end of search, consider move.

- As a conversion option, now look for itdesc words to add to empty tags (or do they have
  to be empty?) I think this is tag-specific: if I find a color word I can feel pretty
  good about adding it to the color tag even if a diff value is already ther.

- Where there is no noun, it would be fun to take known nouns in descending order of popularity
and look for them in itdescs. Here we need to be more lenient on edit-distance, balanced against 
length of string. Do we specify a max-len from min of two terms? The shorter the term the lower 
the confidence.

[more to come]

|#



