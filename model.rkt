#lang racket
(require redex/reduction-semantics "typeset.rkt")
(provide ax1 lang1 lang2)

(define-language L
  (b ::= tt ff)
  (e ::= b (and e e))

  (E ::= hole (and E e) (and e E)))

(define-judgment-form L
  #:mode (ax-->1 I O)
  #:contract (ax-->1 e e)

  [------------------
   (ax-->1 (and tt tt) tt)]

  [------------------
   (ax-->1 (and ff tt) ff)]

  [------------------
   (ax-->1 (and tt ff) ff)]

  [------------------
   (ax-->1 (and ff ff) ff)])

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


(define ax1 (jf->pict ax-->1))
(define lang1 (grammar->pict L))
(define lang2 (grammar->pict L #:nts '(E)))
