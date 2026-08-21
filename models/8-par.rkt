#lang racket
(require redex/reduction-semantics
         (prefix-in 7: "7-cont.rkt"))
(provide L -->ax --> -->* -->traces)

(define-extended-language L 7:L
  (e ::=
     ....
     (par e e)
     )
  (E ::=
     ....
     (par e E)
     (par E e))

  (x ::= variable-not-otherwise-mentioned))

(define-overriding-judgment-form L 7:-->ax
  #:mode (-->ax I I O O)
  #:contract (-->ax Σ e Σ e)
)

(define-overriding-judgment-form L 7:-->
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

(define-judgment-form L
  #:mode (-->traces I O)
  #:contract (-->traces (Σ e) (Σ e))
  [(--> Σ_1 e_1 Σ_2 e_2)
   -----
   (-->traces (Σ_1 e_1) (Σ_2 e_2))])

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

(test-judgment-holds (-->* · (let x (par x (seq (set x) x)))
                           (⊕ x = tt ·) (par tt tt)))
(test-judgment-holds (-->* · (let x (par x (seq (set x) x)))
                           (⊕ x = tt ·) (par ff tt)))
