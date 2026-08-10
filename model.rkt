#lang racket
(require redex/reduction-semantics pict "typeset.rkt")
(provide ax1 lang1 lang2)

(define-language L
  (b ::= tt ff)
  (e ::= b (and e e))

  (E ::= hole (and E e) (and e E)))

(define-judgment-form L
  #:mode (ax-->1 I O)
  #:contract (ax-->1 e e)

  [------------------ "andtt"
   (ax-->1 (and tt tt) tt)]

  [------------------ "andft"
   (ax-->1 (and ff tt) ff)]

  [------------------ "andtf"
   (ax-->1 (and tt ff) ff)]

  [------------------ "andff"
   (ax-->1 (and ff ff) ff)]

  [------------------ "ortt"
   (ax-->1 (or tt tt) tt)]

  [------------------ "orft"
   (ax-->1 (or ff tt) tt)]

  [------------------ "ortf"
   (ax-->1 (or tt ff) tt)]

  [------------------ "orff"
   (ax-->1 (or ff ff) ff)]

  [------------------ "nott"
   (ax-->1 (not tt) ff)]

  [------------------ "notf"
   (ax-->1 (not ff) tt)])

(define-judgment-form L
  #:mode (ax--> I O)
  #:contract (ax--> e e)

  [------------------
   (ax--> (and tt e) e)]

  [------------------
   (ax--> (and ff e) ff)]

  [------------------
   (ax--> (or tt e) tt)]

  [------------------
   (ax--> (or ff e) e)]

  [------------------
   (ax--> (not ff) tt)]

  [------------------
   (ax--> (not tt) ff)])

(define-judgment-form L
  #:mode (--> I O)
  #:contract (--> e e)

  [(ax--> e_1 e_2)
   ------
   (--> (in-hole E e_1) (in-hole E e_2))])


(test-judgment-holds (--> (and ff tt)
                          ff))


(define ax1 (htl-append 100
                        (jf->pict ax-->1 #:cases '("andtt" "andtf" "andft" "andff"))
                        (jf->pict ax-->1 #:cases '("ortt" "ortf" "orft" "orff"))
                        (jf->pict ax-->1 #:cases '("nott" "notf"))))
(define lang1 (grammar->pict L))
(define lang2 (grammar->pict L #:nts '(E)))
