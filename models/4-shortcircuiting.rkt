#lang racket
(require redex/reduction-semantics)
(provide L -->ax --> -->*)

(define-language L
  (e ::=
     (and e e)
     (or e e)
     (not e)
     (let x e)
     (set x)
     x
     (seq e e)
     b)
  (b ::= tt
     ff)

  (E ::= hole
     (and E e)
     (or E e)
     (not E)
     (seq E e))
  (v ::= b)

  (Σ ::= · (⊕ x = v Σ))
  
  (x ::= variable-not-otherwise-mentioned)
  #:binding-forms
  (let x e))

(define-judgment-form L
  #:mode (-->ax I I O O)
  #:contract (-->ax Σ e Σ e)

  [------------------ "andt"
   (-->ax Σ (and tt e) Σ e)]

  [------------------ "andf"
   (-->ax Σ (and ff e) Σ ff)]

  [------------------ "ort"
   (-->ax Σ (or tt e) Σ e)]

  [------------------ "orf"
   (-->ax Σ (or ff e) Σ ff)]

  [------------------ "nott"
   (-->ax Σ (not tt) Σ ff)]

  [------------------ "notf"
   (-->ax Σ (not ff) Σ tt)]

  [------------------ "let"
   (-->ax Σ (let x e) (extend Σ x ff) e)]

  [------------------ "set"
   (-->ax Σ (set x) (extend Σ x tt) tt)]

  [------------------ "get"
   (-->ax Σ x Σ (lookup Σ x))]

  [------------------ "seq"
   (-->ax Σ (seq v e) Σ e)])

(define-judgment-form L
  #:mode (--> I I O O)
  #:contract (--> Σ e Σ e)

  [(-->ax Σ_1 e_1 Σ_2 e_2)
   ------ "ax"
   (--> Σ_1 (in-hole E e_1) Σ_2 (in-hole E e_2))])

(define-judgment-form L
  #:mode (-->* I I O O)
  #:contract (-->* Σ e Σ e)

  [------ "refl"
   (-->* Σ e Σ e)]

  [(--> Σ_1 e_1 Σ_2 e_2) (-->* Σ_2 e_2 Σ_3 e_3)
   ------ "step"
   (-->* Σ_1 e_1 Σ_3 e_3)])

(define-metafunction L
  extend : Σ x b -> Σ
  [(extend (⊕ x = b_1 Σ) x b_2) (⊕ x = b_2 Σ)]
  [(extend (⊕ x_1 = b_1 Σ) x_2 b_2) (⊕ x_1 = b_1 (extend Σ x_2 b_2))]
  [(extend · x b) (⊕ x = b ·)])

(define-metafunction L
  lookup : Σ x -> b
  [(lookup (⊕ x = b Σ) x) b]
  [(lookup (⊕ x_1 = b Σ) x_2) (lookup Σ x_2)])

(test-judgment-holds (-->ax ·
                            (let x (or x tt))
                            (⊕ x = ff ·)
                            (or x tt)))

(test-judgment-holds (-->ax (⊕ x = ff ·)
                            x
                            (⊕ x = ff ·)
                            ff))

(test-judgment-holds (-->* ·
                           (let x (and x (seq (set x) tt)))
                           (⊕ x = ff ·)
                           ff))

(test-equal (judgment-holds (-->* ·
                                  (let x (and x (seq (set x) tt)))
                                  (⊕ x = tt ·)
                                  tt))
            #f)
