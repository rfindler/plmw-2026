#lang racket
(require redex/reduction-semantics pict "typeset.rkt")
(provide ax1 ax2 ax3 lang1 lang2 lang3 nested_example)

(define-language L
  (e ::=
     (and e e)
     (or e e)
     (not e)
     b)
  (b ::= tt
     ff)

  (E ::= hole
     (and E e) (and e E)
     (or E e) (or e E)
     (not E)))

(define-extended-language L2 L
  (e ::= ....
     (let x e)
     (set x)
     (seq e e))
  (x ::= variable-not-otherwise-mentioned)
  #:binding-forms
  (let x e))

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
  #:mode (ax-->2 I I O O)
  #:contract (ax-->2 Σ e Σ e)

  [------------------ "andtt"
   (ax-->2 Σ (and tt tt) Σ tt)]

  [------------------ "andft"
   (ax-->2 Σ (and ff tt) Σ ff)]

  [------------------ "andtf"
   (ax-->2 Σ (and tt ff) Σ ff)]

  [------------------ "andff"
   (ax-->2 Σ (and ff ff) Σ ff)]

  [------------------ "ortt"
   (ax-->2 Σ (or tt tt) Σ tt)]

  [------------------ "orft"
   (ax-->2 Σ (or ff tt) Σ tt)]

  [------------------ "ortf"
   (ax-->2 Σ (or tt ff) Σ tt)]

  [------------------ "orff"
   (ax-->2 Σ (or ff ff) Σ ff)]

  [------------------ "nott"
   (ax-->2 Σ (not tt) Σ ff)]

  [------------------ "notf"
   (ax-->2 Σ (not ff) Σ tt)]

  [------------------ "let"
   (ax-->2 Σ (let x e) (extend Σ x ff) e)]

  [------------------ "set"
   (ax-->2 Σ (set x) (extend Σ x tt) tt)]

  [------------------ "seq"
   (ax-->2 Σ (seq b e) Σ e)])

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
(define lang1 (grammar->pict L #:nts '(e b)))
(define nested_example (t->pict L (or (and tt ff)
                                      tt)))
(define lang2 (grammar->pict L2 #:nts '(e b)))
(define lang3 (grammar->pict L #:nts '(E)))

(define ax2 (htl-append 60
                        (jf->pict ax-->2 #:cases '("let" "set" "seq"))))

(define ax3 (htl-append 60
                        (jf->pict ax-->2 #:cases '("andtt" "andtf" "andft" "andff"))
                        (jf->pict ax-->2 #:cases '("ortt" "ortf" "orft" "orff"))
                        (jf->pict ax-->2 #:cases '("nott" "notf"))))
