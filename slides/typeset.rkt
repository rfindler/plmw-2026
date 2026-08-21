#lang racket
(require racket/gui/base redex/pict pict redex/reduction-semantics)
(provide jf->pict grammar->pict t->pict str_to_tt libertine font_size)

(define font_size 32)

(define-syntax-rule
  (define-font id name)
  (define id (define-font/proc name)))
(define (define-font/proc name)
  (unless (member name (get-face-list))
    (eprintf "missing font ~a\n" name))
  name)

(define-font libertine "Caladea" #; "Baskerville" #;"Linux Libertine O")
(define-font inconsolata '"Courier Prime" #;"Inconsolata")

(define body-text-style inconsolata)
(define tt-text-style inconsolata)

(define (dtext str #:subscript? [subscript? #f])
  ((current-text) str
                  (if subscript?
                      (cons 'subscript (literal-style))
                      (literal-style))
                  (default-font-size)))

(define (mtext str
               #:subscript? [subscript? #f]
               #:superscript? [superscript? #f]
               #:bold? [bold? #f])
  (define nonbold-style
    (cond
      [subscript?
       (cons 'subscript (default-style))]
      [superscript?
       (cons 'superscript (default-style))]
      [else (default-style)]))
  ((current-text) str
                  (if bold?
                      (cons 'bold nonbold-style)
                      nonbold-style)
                  (default-font-size)))

(define (⟩-pict) (mtext "⟩"))
(define (⟨-pict) (mtext "⟨"))
(define the-arrow #;"→" "⟶")

(define (-->ax-rewrite lws)
  (match lws
    [(list _open _--> e1 e2 _close)
     (generic-arrow-rewrite (list e1) (list e2) "ax" #f)]
    [(list _open _--> Σ1 e1 Σ2 e2 _close)
     (generic-arrow-rewrite (list Σ1 e1) (list Σ2 e2) "ax" #f)]))
(define (-->-rewrite lws)
  (match lws
    [(list _open _--> Σ1 e1 Σ2 e2 _close)
     (generic-arrow-rewrite (list Σ1 e1) (list Σ2 e2) "" #f)]))
(define (-->*-rewrite lws)
  (match lws
    [(list _open _--> Σ1 e1 Σ2 e2 _close)
     (generic-arrow-rewrite (list Σ1 e1) (list Σ2 e2) "*" #f)]))

(define (generic-arrow-rewrite before-items after-items subscript star?)
  (define before-brackets? (not (= (length before-items) 1)))
  (define after-brackets? (not (= (length after-items) 1)))
  (append
   (if before-brackets?
       (list "" (just-before (⟨-pict) (first before-items)))
       (list ""))
   (add-between before-items " , ")
   (list (just-after (hbl-append (if before-brackets?
                                     (⟩-pict)
                                     (blank))
                                 (mtext (~a " " the-arrow))
                                 (if subscript (mtext subscript #:subscript? #t) (blank))
                                 (if star? (mtext "*") (blank)))
                     (last before-items)))
   (if after-brackets?
       (list (just-before (⟨-pict) (first after-items)))
       '())
   (add-between after-items " , ")
   (if after-brackets?
       (list (just-after (⟩-pict) (last after-items)))
       '())))

(define (extend-rewrite lws)
  (list ""
        (list-ref lws 2)
        " ⊕ { "
        (list-ref lws 3)
        " = "
        (list-ref lws 4)
        " }"))

(define (⊕-rewrite lws)
  (match-define (list _open _⊕ x _= b Σ _close) lws)
  (unless (and (lw? Σ)
               (equal? (lw-e Σ) '·))
    (error '⊕-rewrite "cannot rewrite bigger stores"))
  (list "{"
        x
        "="
        b
        "}"))

(define (∉Can⊥-rewrite lws)
  (match-define (list _open _∉ x Σ e _close) lws)
  (list ""
        x
        " ∉ Can⊥("
        Σ
        " , "
        e
        ")"))

(define (substitute-rewrite lws)
  (match-define (list _open _substitute e x v _close) lws)
  (list "" e "{" x " := " v "}"))

(define (lookup-rewrite lws)
  (list ""
        (list-ref lws 2)
        "("
        (list-ref lws 3)
        ")"))

(define (lookup-⊥-rewrite lws)
  (list ""
        (list-ref lws 2)
        "("
        (list-ref lws 3)
        ") = ⊥"))

(define (no-catch-rewrite lws)
  (match-define (list _open _no-catch E _close) lws)
  (list (hbl-append (mtext "∀ E. (")
                    (dtext "catch")
                    (mtext " ")
                    (mtext "E")
                    (mtext ")")
                    (mtext " ∉ "))
        E
        ""))

(define (with-the-rewriters thunk)
  (with-compound-rewriters
      (['-->ax -->ax-rewrite]
       ['--> -->-rewrite]
       ['-->* -->*-rewrite]
       ['no-catch no-catch-rewrite]
       ['extend extend-rewrite]
       ['lookup lookup-rewrite]
       ['substitute substitute-rewrite]
       ['lookup-⊥ lookup-⊥-rewrite]
       ['∉Can⊥ ∉Can⊥-rewrite]
       ['⊕ ⊕-rewrite])
    (parameterize ([label-font-size font_size]
                   [default-font-size font_size]
                   [metafunction-font-size font_size]
                   [label-style body-text-style]
                   [grammar-style body-text-style]
                   [paren-style tt-text-style]
                   [literal-style tt-text-style]
                   [metafunction-style body-text-style]
                   [non-terminal-style body-text-style]
                   [default-style body-text-style]
                   [non-terminal-subscript-style (cons 'subscript body-text-style)]
                   [non-terminal-superscript-style (cons 'superscript body-text-style)]
                   [judgment-form-no-bar-when-no-premises #t]
                   [premise-spacing 100])
      (thunk))))

(define (jf->pict jf #:cases [cases #f])
  (with-the-rewriters
      (λ ()
        (parameterize ([judgment-form-cases cases])
          (render-judgment-form jf)))))

(define (grammar->pict L #:nts [_nts #f])
  (define nts
    (or _nts
        '(e)))
  (with-the-rewriters
      (λ () (render-language L #:nts nts))))

(define-syntax-rule
  (t->pict L t)
  (with-the-rewriters
      (λ () (term->pict L t))))

(define (str_to_tt str)
  (with-the-rewriters
      (λ ()
        (text str (literal-style) (default-font-size)))))

(module+ model1
  (require "../models/1-bool-everywhere.rkt")
  (provide lang ax ex ex_sexp ex_ctxt example_used_axiom)
  (define lang (grammar->pict L #:nts '(e b)))
  (define ax
    (list (jf->pict -->ax #:cases '("andtt" "andtf" "andft" "andff"))
          (jf->pict -->ax #:cases '("ortt" "ortf" "orft" "orff"))
          (jf->pict -->ax #:cases '("nott" "notf"))))

  (define example_used_axiom
    (jf->pict -->ax #:cases '("andtf")))

  (define-values (ex ex_sexp)
    (pict-and-sexp
     L
     (or (and tt ff)
         tt)))

  (define-values (ex_ctxt ex_ctxt_sexp)
    (pict-and-sexp
     L
     (or hole
         tt)))

  (unless (equal? (plug ex_ctxt_sexp (term (and tt ff)))
                  ex_sexp)
    (error 'didnt-plug.model1)))

(define-syntax-rule
  (pict-and-sexp L e)
  (values (t->pict L e)
          (term e)))

(module+ model2
  (require "../models/2-bool-state-everywhere.rkt")
  (provide lang schema new_ax bool_ax
           ex ex_sexp
           ex_st1 ex_st2
           ex_1 ex_2 ex_3 ex_4 ex_5 ex_6
           let_ax set_ax get_ax seq_ax
           ex_ctxt)
  (define lang (grammar->pict L #:nts '(e b)))
  (define schema
    (frame (inset (vr-append
                   (t->pict L (-->ax Σ e Σ e))
                   (hbl-append 6
                               (t->pict L Σ)
                               (t->pict L ∈)
                               (t->pict L x)
                               (lift-above-baseline (t->pict L ⇴) 2)
                               (t->pict L b)))
                  20 10)))
  (define new_ax (jf->pict -->ax #:cases '("let" "set" "get" "seq")))
  (define bool_ax
    (list (jf->pict -->ax #:cases '("andtt" "andtf" "andft" "andff"))
          (jf->pict -->ax #:cases '("ortt" "ortf" "orft" "orff"))
          (jf->pict -->ax #:cases '("nott" "notf"))))

  (define let_ax (jf->pict -->ax #:cases '("let")))
  (define set_ax (jf->pict -->ax #:cases '("set")))
  (define get_ax (jf->pict -->ax #:cases '("get")))
  (define seq_ax (jf->pict -->ax #:cases '("seq")))

  (define-values (ex ex_sexp)
    (pict-and-sexp
     L
     (let x
       (and x
            (seq (set x)
                 tt)))))
  
  (define-values (ex_1 ex_1_sexp)
    (pict-and-sexp L (and x
                          (seq (set x)
                               tt))))

  (define-values (ex_ctxt ex_ctxt_sexp)
    (pict-and-sexp
     L
     (and hole
          (seq (set x)
               tt))))

  (unless (equal? (plug ex_ctxt_sexp (term x))
                  ex_1_sexp)
    (error 'didnt-plug.model2 "\n  ~s\n  ~s"
           (plug ex_ctxt_sexp (term x))
           ex_1_sexp))

  (define ex_2 (t->pict L (and ff
                               (seq (set x)
                                    tt))))
  (define ex_3 (t->pict L ff))
  (define ex_4 (t->pict L (and x
                               (seq tt
                                    tt))))
  (define ex_5 (t->pict L (and x
                               tt)))
  (define ex_6 (t->pict L tt))

  (define ex_st1 (t->pict L (⊕ x = ff ·)))
  (define ex_st2 (t->pict L (⊕ x = tt ·))))

(module+ model3
  (require "../models/3-bool-state-left-to-right.rkt")
  (provide ec_lang)

  (define ec_lang (grammar->pict L #:nts '(E v))))

(module+ model4
  (require "../models/4-shortcircuiting.rkt")
  (provide ax ec_lang red decomp)
  (define ec_lang (grammar->pict L #:nts '(E)))
  (define ax (list
              (jf->pict -->ax #:cases '("andt" "andf"))
              (jf->pict -->ax #:cases '("ort" "orf"))))
  (define red (jf->pict -->))
  (define decomp (t->pict L (in-hole E e))))

(module+ model5
  (require (prefix-in cbv: "../models/5-cbv.rkt")
           (prefix-in cbn: "../models/5-cbn.rkt"))
  (provide ax_cbv ec_cbv ax_cbn ec_cbn)
  (define ax_cbv (jf->pict cbv:-->ax))
  (define ec_cbv (grammar->pict cbv:L #:nts '(E v)))
  (define ax_cbn (jf->pict cbn:-->ax))
  (define ec_cbn (grammar->pict cbn:L #:nts '(E v))))

(module+ model6
  (require "../models/6-exn.rkt")
  (provide red lang_e lang_ec)
  (define lang_e (grammar->pict L #:nts '(e)))
  (define lang_ec (grammar->pict L #:nts '(E)))
  (define red (list (jf->pict --> #:cases '("ax"))
                    (jf->pict --> #:cases '("throw"))
                    (jf->pict --> #:cases '("catch")))))

(module+ model7
  (require "../models/7-cont.rkt")
  (provide red lang_e lang_ec)
  (define lang_e (grammar->pict L #:nts '(e)))
  (define lang_ec (grammar->pict L #:nts '(E)))
  (define red (list (jf->pict --> #:cases '("call/cc"))
                    (jf->pict --> #:cases '("abort")))))

(module+ model8
  (require "../models/8-par.rkt")
  (provide red lang_e lang_ec redex_state_to_pict step
           ex ex_and_store_sexp)
  (define lang_e (grammar->pict L #:nts '(e)))
  (define lang_ec (grammar->pict L #:nts '(E)))
  (define-values (ex ex_sexp)
    (pict-and-sexp
     L
     (let x (par x (seq (set x) x)))))
  (define ex_and_store_sexp (list (term ·) ex_sexp))
  (define red (jf->pict -->ax #:cases '("par")))
  (define (redex_state_to_pict t)
    (define p
      (with-the-rewriters
          (λ ()
            (htl-append (⟨-pict)
                        (term->pict/pretty-write L (car t))
                        (mtext " , ")
                        (term->pict/pretty-write L (cadr t) #:width 20)
                        (⟩-pict)))))
    (define space 10)
    (cc-superimpose
     (colorize (linewidth 4
                          (rectangle (+ (pict-width p) space)
                                     (+ (pict-height p) space)))
               "gray")
     p))
  (define (step t)
    (apply-reduction-relation -->traces t)))

(module+ model9
  (require "../models/9-esterel.rkt")
  (provide red)
  (define red
    (list (jf->pict -->ax #:cases '("let"))
          (jf->pict -->ax #:cases '("set"))
          (jf->pict -->ax #:cases '("get"))
          (jf->pict --> #:cases '("cannot"))
          )))
