#lang racket
(require redex/reduction-semantics)
(provide L -->ax)

(define-language L
  (e ::=
     (and e e)
     (or e e)
     (not e)
     b)
  (b ::= tt
     ff)

  (C ::= hole
     (and C e)
     (and e C)
     (or C e)
     (or e C)
     (not C)))

(define-judgment-form L
  #:mode (-->ax I O)
  #:contract (-->ax e e)

  [------------------ "andtt"
   (-->ax (and tt tt) tt)]

  [------------------ "andft"
   (-->ax (and ff tt) ff)]

  [------------------ "andtf"
   (-->ax (and tt ff) ff)]

  [------------------ "andff"
   (-->ax (and ff ff) ff)]

  [------------------ "ortt"
   (-->ax (or tt tt) tt)]

  [------------------ "orft"
   (-->ax (or ff tt) tt)]

  [------------------ "ortf"
   (-->ax (or tt ff) tt)]

  [------------------ "orff"
   (-->ax (or ff ff) ff)]

  [------------------ "nott"
   (-->ax (not tt) ff)]

  [------------------ "notf"
   (-->ax (not ff) tt)])


(define-judgment-form L
  #:mode (--> I O)
  #:contract (--> e e)

  [(-->ax e_1 e_2)
   ------------------------------------- "ax"
   (--> (in-hole C e_1) (in-hole C e_2))])

(define-judgment-form L
  #:mode (-->* I O)
  #:contract (-->* e e)

  [---------- "refl"
   (-->* e e)]

  [(--> e_1 e_2) (-->* e_2 e_3)
   ---------------------------- "step"
   (-->* e_1 e_3)])

(test-judgment-holds (-->* (not (and (or ff tt)
                                     (or tt tt)))
                           ff))
