#lang racket
(require redex/reduction-semantics
         (prefix-in 4: "4-shortcircuiting.rkt"))
(provide L -->ax --> -->*)

(define-extended-language L 4:L
  (e ::=
     ....
     (λ x e)
     (e e))

  (E ::=
     ....
     (E e))
  (v ::=
     ....
     (λ x e))

  #:binding-forms
  (let x e)
  (λ x e))

(define-extended-judgment-form L 4:-->ax
  #:mode (-->ax I I O O)
  #:contract (-->ax Σ e Σ e)

  [------------------ "cbv"
   (-->ax Σ ((λ x e_1) e_2)
          Σ (substitute e_1 x e_2))])

(define-overriding-judgment-form L 4:-->
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

(test-judgment-holds (-->* · ((λ x x) tt) · tt))
(test-judgment-holds (-->* · ((((λ x (λ y x)) (λ z z)) tt) ff) · ff))
(test-judgment-holds (-->* · ((λ x ff) ((λ x (x x)) (λ x (x x)))) · ff))
