#lang racket
(require redex/reduction-semantics pict "typeset.rkt")
(provide ax1 ax2 ax3 lang1 lang2 lang3
         ex1 ex1_sexp
         ex2 ex2_sexp ex2_1 ex2_2 ex2_3 ex2_4 ex2_5 ex2_6 ex2_st1 ex2_st2)

(define-language L
  (e ::=
     (and e e)
     (or e e)
     (not e)
     b)
  (b ::= tt
     ff)

  (C ::= hole
     (and C e) (and e C)
     (or C e) (or e C)
     (not C)
     (seq C e))

  (E ::= hole
     (and E e) (and b E)
     (or E e) (or b E)
     (not E)
     (seq E e))

  (Σ ::= · (⊕ x = b Σ))
  
  (x ::= variable-not-otherwise-mentioned)
  #:binding-forms
  (let x e))

(define-extended-language L2 L
  (e ::= ....
     (let x e)
     (set x)
     x
     (seq e e)))

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

(define-judgment-form L2
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

  [------------------ "get"
   (ax-->2 Σ x Σ (lookup Σ x))]

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

(define-judgment-form L2
  #:mode (-->C I I O O)
  #:contract (-->C Σ e Σ e)

  [(ax-->2 Σ_1 e_1 Σ_2 e_2)
   ------
   (-->C Σ_1 (in-hole C e_1) Σ_2 (in-hole C e_2))])

(define-judgment-form L2
  #:mode (-->C* I I O O)
  #:contract (-->C* Σ e Σ e)

  [------
   (-->C* Σ e Σ e)]

  [(-->C Σ_1 e_1 Σ_2 e_2) (-->C* Σ_2 e_2 Σ_3 e_3)
   ------
   (-->C* Σ_1 e_1 Σ_3 e_3)])

(define-judgment-form L2
  #:mode (-->E I I O O)
  #:contract (-->E Σ e Σ e)

  [(ax-->2 Σ_1 e_1 Σ_2 e_2)
   ------
   (-->E Σ_1 (in-hole E e_1) Σ_2 (in-hole E e_2))])

(define-judgment-form L2
  #:mode (-->E* I I O O)
  #:contract (-->E* Σ e Σ e)

  [------
   (-->E* Σ e Σ e)]

  [(-->E Σ_1 e_1 Σ_2 e_2) (-->E* Σ_2 e_2 Σ_3 e_3)
   ------
   (-->E* Σ_1 e_1 Σ_3 e_3)])


(define-metafunction L2
  extend : Σ x b -> Σ
  [(extend (⊕ x = b_1 Σ) x b_2) (⊕ x = b_2 Σ)]
  [(extend (⊕ x_1 = b_1 Σ) x_2 b_2) (⊕ x_1 = b_1 (extend Σ x_2 b_2))]
  [(extend · x b) (⊕ x = b ·)])

(define-metafunction L2
  lookup : Σ x -> b
  [(lookup (⊕ x = b Σ) x) b]
  [(lookup (⊕ x_1 = b Σ) x_2) (lookup Σ x_2)])

(test-judgment-holds (ax-->2 ·
                             (let x (or x tt))
                             (⊕ x = ff ·)
                             (or x tt)))

(test-judgment-holds (ax-->2 (⊕ x = ff ·)
                             x
                             (⊕ x = ff ·)
                             ff))



(test-judgment-holds (-->C ·
                           (and ff tt)
                           ·
                           ff))

(test-judgment-holds (-->C* ·
                            (let x (and x (seq (set x) tt)))
                            (⊕ x = tt ·)
                            ff))

(test-judgment-holds (-->C* ·
                            (let x (and x (seq (set x) tt)))
                            (⊕ x = tt ·)
                            tt))

(test-judgment-holds (-->E* ·
                            (let x (and x (seq (set x) tt)))
                            (⊕ x = tt ·)
                            ff))

(test-equal (judgment-holds (-->E* ·
                                   (let x (and x (seq (set x) tt)))
                                   (⊕ x = tt ·)
                                   tt))
            #f)

(define ax1 (htl-append 100
                        (jf->pict ax-->1 #:cases '("andtt" "andtf" "andft" "andff"))
                        (jf->pict ax-->1 #:cases '("ortt" "ortf" "orft" "orff"))
                        (jf->pict ax-->1 #:cases '("nott" "notf"))))
(define lang1 (grammar->pict L #:nts '(e b)))
(define ex1 (t->pict L (or (and tt ff)
                           tt)))
(define ex1_sexp '(or (and tt ff) tt))
(define lang2 (grammar->pict L2 #:nts '(e b)))
(define lang3 (grammar->pict L #:nts '(C)))

(define ax2 (vr-append 60
                       (frame (inset (vr-append
                                      (t->pict L (ax-->2 Σ e Σ e))
                                      (hbl-append 6
                                                  (t->pict L Σ)
                                                  (t->pict L ∈)
                                                  (t->pict L x)
                                                  (lift-above-baseline (t->pict L ⇴) 2)
                                                  (t->pict L b)))
                                     10))
                       (jf->pict ax-->2 #:cases '("let" "set" "get" "seq"))))

(define ex2 (t->pict L2 (let x
                          (and x
                               (seq (set x)
                                    tt)))))
(define ex2_sexp '(let x
                    (and x
                         (seq (set x)
                              tt))))
(define ex2_1 (t->pict L2 (and x
                               (seq (set x)
                                    tt))))
(define ex2_2 (t->pict L2 (and ff
                               (seq (set x)
                                    tt))))
(define ex2_3 (t->pict L2 ff))
(define ex2_4 (t->pict L2 (and x
                               (seq tt
                                    tt))))
(define ex2_5 (t->pict L2 (and x
                               tt)))
(define ex2_6 (t->pict L2 tt))
(define ex2_st1 (t->pict L2 (⊕ x = ff ·)))
(define ex2_st2 (t->pict L2 (⊕ x = tt ·)))

(define ax3 (htl-append 60
                        (jf->pict ax-->2 #:cases '("andtt" "andtf" "andft" "andff"))
                        (jf->pict ax-->2 #:cases '("ortt" "ortf" "orft" "orff"))
                        (jf->pict ax-->2 #:cases '("nott" "notf"))))
