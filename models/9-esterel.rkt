#lang racket
(require redex/reduction-semantics
         (prefix-in 8: "8-par.rkt"))
(provide L -->ax --> -->*)

(define-extended-language L 8:L
  (Σ ::= · (⊕ x = b⊥ Σ))
  (b⊥ ::= b ⊥)

  (x ::= variable-not-otherwise-mentioned))

(define-overriding-judgment-form L 8:-->ax
  #:mode (-->ax I I O O)
  #:contract (-->ax Σ e Σ e)

  [------------------ "let"
   (-->ax Σ (let x e) (extend Σ x ⊥) e)]

  [------------------ "set"
   (-->ax Σ (set x) (extend Σ x tt) tt)]

  [(where v (lookup Σ x))
   ------------------ "get"
   (-->ax Σ x Σ v)])

;; this is (woefully) incomplete
(define-judgment-form L
  #:mode (∉Can⊥ I I I)
  #:contract (∉Can⊥ x Σ e)
  [(where ⊥ (lookup Σ x))
   ------
   (∉Can⊥ x Σ x)])

(define-metafunction L
  lookup : Σ x -> b⊥
  [(lookup (⊕ x = b Σ) x) b]
  [(lookup (⊕ x = ⊥ Σ) x) ⊥]
  [(lookup (⊕ x_1 = b Σ) x_2) (lookup Σ x_2)])

(define-judgment-form L
  #:mode (lookup-⊥ I O)
  #:contract (lookup-⊥ Σ x)

  [-----
   (lookup-⊥ (⊕ x = ⊥ Σ) x)]
  [(lookup-⊥ Σ x)
   -----
   (lookup-⊥ (⊕ x = b Σ) x)])

(define-metafunction L
  extend : Σ x b⊥ -> Σ
  [(extend (⊕ x = b⊥_1 Σ) x b⊥_2) (⊕ x = b⊥_2 Σ)]
  [(extend (⊕ x_1 = b⊥_1 Σ) x_2 b⊥_2) (⊕ x_1 = b⊥_1 (extend Σ x_2 b⊥_2))]
  [(extend · x b⊥) (⊕ x = b⊥ ·)])

(define-overriding-judgment-form L 8:-->
  #:mode (--> I I O O)
  #:contract (--> Σ e Σ e)

  [(-->ax Σ_1 e_1 Σ_2 e_2)
   ------ "ax"
   (--> Σ_1 (in-hole E e_1) Σ_2 (in-hole E e_2))]

  [(lookup-⊥ Σ x) (blocked Σ e) (∉Can⊥ x Σ e)
   ------------------ "cannot"
   (--> Σ e (extend Σ x ff) e)])

(define-judgment-form L
  #:mode (blocked I I)
  #:contract (blocked Σ e)

  [(blocked Σ e_1) (blocked Σ e_2)
   -------
   (blocked Σ (par e_1 e_2))]

  [(blocked Σ e)
   ----
   (blocked Σ (par e v))]

  [(blocked Σ e)
   ----
   (blocked Σ (par v e))]

  [(blocked Σ e)
   ----
   (blocked Σ (and v e))]

  [(blocked Σ e_1)
   ----
   (blocked Σ (and e_1 e_2))]

  [(blocked Σ e)
   ----
   (blocked Σ (or v e))]

  [(blocked Σ e_1)
   ----
   (blocked Σ (or e_1 e_2))]

  [(blocked Σ e_1)
   ----
   (blocked Σ (seq e_1 e_2))]

  [(blocked Σ e)
   ----
   (blocked Σ (not e))])

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
                            (⊕ x = ⊥ ·)
                            (or x tt)))

(test-judgment-holds (-->ax (⊕ x = ff ·)
                            x
                            (⊕ x = ff ·)
                            ff))

(test-judgment-holds (-->* ·
                           (let x (and x (seq (set x) tt)))
                           (⊕ x = ⊥ ·)
                           (and x (seq (set x) tt))))

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

