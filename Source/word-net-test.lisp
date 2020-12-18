(in-package :wordnet)

#+test
(cd-report t 100)

;; issues
;; collection -- request for money
;; cotton - take a liking to
;; culture - society (lost good def, picked up bad)
;; head -- too many defs
;; hole -- golf
;; latex just paint
;; level, lighted, low, pole, ring, set, sleeve, tip, tool

#+test
(time-set)

(defun time-set ()
  (word-net-reset)
  (progn ;; with-metrics (t t "ghxdef set")
   (print (ghx-definitions "set"))))

;;;Starting timing run of  ghxdef set
;;;0> bingo sense V 1690974 ("adapt for performance in a different way")
;;;; cpu time (non-gc) 4,593 msec user, 0 msec system
;;;; cpu time (gc)     250 msec user, 0 msec system
;;;; cpu time (total)  4,843 msec user, 0 msec system
;;;; real time  4,843 msec
;;;; space allocation:
;;;;  2,087,672 cons cells, 118,597,040 other bytes, 0 static bytes
;;;Above timing was of  ghxdef set
;;;Counts after: clearp T, length 2: ("ghxdef set")
;;;11111 ... 11111 ... (:MAPVALUE)
;;;11112 ...  1 ... (:TGTNILEVAL

;;;---after
;;;count-clear > (ghxdef set)
;;;Starting timing run of  ghxdef set
;;;; cpu time (non-gc) 63 msec user, 0 msec system
;;;; cpu time (gc)     0 msec user, 0 msec system
;;;; cpu time (total)  63 msec user, 0 msec system
;;;; real time  62 msec
;;;; space allocation:
;;;;  20 cons cells, 630,616 other bytes, 0 static bytes
;;;Above timing was of  ghxdef set

#+test
(explore-word )

(defun explore-word (&aux (w "safety"))
  (test-setup)
  (word-net-reset)
  #+chilll
  (client-dictionary-open-ensure)
  (ghx-reference-open-ensure)
  #+shhh
  (with-db (*client-dictionary*)
    (trcx itdescs-follow)
    (let ((cw (db-get 'cn-word 'text (up$ w))))
      (loop for i in (client-items cw) do
            (print (itdesc i)))
      (trcx used-in (length (client-items cw)))))

  (word-leaves w :dos 2 :types '(hyponym) ;:poses '(n)
    :not-types nil #+not +word-net-down-concepts+)

  #+shhhh
  (progn
    (trcx ghx-definitions-follow)
    (print (ghx-definitions w t)))
  ;;;  (trcx all-defintions-follow)
  ;;;  (print (word-net-lookup w))
  #+regress
  (loop for word in '(joint absorb aorta plate balloon)
      do (print (cons word (ghx-definitions (down$ word))))))

#+test
(with-db (*ghx-reference*)
  (let ((c 'word-net-noun-synset)
        (offs '(5158749 5220461)))
    (loop for off in offs
        collecting (db-get* c 'offset off))))

#+test
(count-ac-class 'word-net-noun-synset)


