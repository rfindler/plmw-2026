#lang racket
(require redex/reduction-semantics
         (prefix-in 5: "5-cbv.rkt"))
(provide L -->ax --> -->*)

(define-extended-language L 5:L
  (e ::=
     ....
     (catch e)
     (throw e)
     )

  (E ::=
     ....
     (catch E)
     (throw E)))

(define-extended-judgment-form L 5:-->ax
  #:mode (-->ax I I O O)
  #:contract (-->ax Σ e Σ e)
)

(define-judgment-form L
  #:mode (--> I I O O)
  #:contract (--> Σ e Σ e)

  [(-->ax Σ_1 e_1 Σ_2 e_2)
   ------ "ax"
   (--> Σ_1 (in-hole E e_1) Σ_2 (in-hole E e_2))]

  [(no-catch E_2)
   ------ "throw"
   (--> Σ (in-hole E_1 (catch (in-hole E_2 (throw v)))) Σ (in-hole E_1 v))]

  [------------------ "catch"
   (--> Σ (in-hole E (catch v)) Σ (in-hole E v))])

(define-judgment-form L
  #:mode (no-catch I)
  #:contract (no-catch E)

  [----
   (no-catch hole)]

  [(no-catch E)
   ----
   (no-catch (throw E))]

  [(no-catch E)
   ----
   (no-catch (and E e))]

  [(no-catch E)
   ----
   (no-catch (or E e))]

  [(no-catch E)
   ----
   (no-catch (not E))]

  [(no-catch E)
   ----
   (no-catch (seq E e))]

  [(no-catch E)
   ----
   (no-catch (E e))]

  [(no-catch E)
   ----
   (no-catch (v E))])

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
