(in-package :wordnet)

;;; --- tag maps ---------------------------------------------------------

(defun tag-maps-list ()
  (let (x)
    (doclass (y 'tag-map)
      (when (typep y 'tag-map)
        (push y x)))
    (nreverse x)))

(let (dbs)
  (defun ensure-map (id &key (type 'ac-map-range)(db *allegrocache*)
                      &aux (id$ (string id)))
    (flet ((emap ()
             (with-db (db)
               (or (find-map id$ :type type)
                 (make-instance type :ac-map-name id$)))))
      (bif (maps (cdr (assoc db dbs)))
        (or (cdr (assoc id maps))
          (let ((map (emap)))
            (rplacd (last maps) (list (cons id map)))
            map))
        (let ((map (emap)))
          (push (cons db (list (cons id map))) dbs)
          map)))))

#+test
(doclass (m 'ac-map-range)
  (bwhen (v (map-value m "o-ring"))
    (trc "BINGO" (ac-map-name m) v)))

#+test
(with-db (*resource-raw*)
  (map-count (retrieve-from-index* 'tag-map 'ac-map-name "COMPOSITION")))

(defun find-tag-map (tag &key (type 'tag-map) (db *allegrocache*) must-find)
  (find-map tag :type type :db db :must-find must-find))

(defun recreate-tag-map (tag &key (type 'tag-map)(db *allegrocache*))
  (assert (not *print-readably*) () "WTF? print readably now on?")
  (bwhen (old (find-map tag :type type :db db))
    (trc "recreate deletes old map" old (type-of old) db)
    (delete-instance old)
    (commit :db db))
  (with-db (db)
    (make-instance type :ac-map-name tag)))

(defclass* tag-map (ac-map-range :print nil))
(defmethod print-object ((self tag-map) s)
  (format s "[M/~a:~d]" (ac-map-name self) (map-count self)))

(defclass* tag-map-by-word (tag-map :print nil))

#+save
(with-db (*clea)
  (loop for m in (tag-maps-list) do (print (list (ac-map-name m)(map-count m)))))

#+test
(progn
  (tag-repair-open-ensure)
  (print (find-tag-map "PROPERTIES" :type 'tag-map-by-word :db *tag-repair*))
  (with-db (*tag-repair*)
    (loop for m in (tag-maps-list) do (print (list (ac-map-name m)(map-count m))))))