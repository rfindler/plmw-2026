#lang racket
(require redex/reduction-semantics
         (prefix-in 6: "6-exn.rkt"))
(provide L -->ax --> -->*)

(define-extended-language L 6:L
  (e ::=
     ....
     call/cc
     (abort e)
     )
  (E ::=
     ....
     (abort E))
  (v ::=
     ....
     call/cc)

  (x ::= variable-not-otherwise-mentioned))

(define-overriding-judgment-form L 6:-->ax
  #:mode (-->ax I I O O)
  #:contract (-->ax Σ e Σ e)
)

(define-overriding-judgment-form L 6:-->
  #:mode (--> I I O O)
  #:contract (--> Σ e Σ e)

  [(-->ax Σ_1 e_1 Σ_2 e_2)
   ------ "ax"
   (--> Σ_1 (in-hole E e_1) Σ_2 (in-hole E e_2))]

  [------ "call/cc"
   (--> Σ (in-hole E (call/cc (λ x e)))
        Σ (in-hole E ((λ x e) (λ x (abort (in-hole E x))))))]

  [------------------ "abort"
   (--> Σ (in-hole E (abort v)) Σ v)])

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
(test-judgment-holds (--> · ((λ x ff) ((λ x (x x)) (λ x (x x)))) · ((λ x ff) ((λ x (x x)) (λ x (x x))))))

(test-judgment-holds (-->* · (catch (throw tt)) · tt))
(test-judgment-holds (-->* · (catch (or (throw tt) ff)) · tt))
(test-judgment-holds (-->* · (catch (or (catch (throw ff)) tt)) · tt))
(test-equal (judgment-holds (-->* · (catch (or (catch (throw ff)) tt))
                                  · ff))
            #f)

(test-judgment-holds (-->* · (call/cc (λ k (and (k tt) ff)))
                           · tt))
(test-judgment-holds (-->* · (and (call/cc (λ k (k tt))) ff)
                           · ff))
