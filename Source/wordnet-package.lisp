(in-package :cl-user)

(defpackage :wordnet
  (:nicknames "WN")

  ;; WordNet API
  (:export 

   ;; finds synset for given string/word
   "LOOKUP-WORD"
   
   ;; finds path between words or synsets
   "FIND-WORDNET-PATH"

   ;; to extract info from synsets
   "GET-WORDS"
   "GET-DEFINITION"
   "GET-RELATED-SYNSETS"

   ;; manually loads wordnet data
   "INITIALIZE-WORDNET"
   ))

