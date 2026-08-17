#lang racket
(require racket/gui/base redex/pict pict)
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

(define body-text-style libertine)
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
  (list "{ "
        x
        " = "
        b
        " }"))

(define (lookup-rewrite lws)
  (list ""
        (list-ref lws 2)
        "("
        (list-ref lws 3)
        ")"))

(define (with-the-rewriters thunk)
  (with-compound-rewriters
      (['-->ax -->ax-rewrite]
       ['--> -->-rewrite]
       ['-->* -->*-rewrite]
       ['extend extend-rewrite]
       ['lookup lookup-rewrite]
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
                   [judgment-form-no-bar-when-no-premises #t])
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
  (provide lang ax ex ex_sexp)
  (define lang (grammar->pict L #:nts '(e b)))
  (define ax
    (list (jf->pict -->ax #:cases '("andtt" "andtf" "andft" "andff"))
          (jf->pict -->ax #:cases '("ortt" "ortf" "orft" "orff"))
          (jf->pict -->ax #:cases '("nott" "notf"))))
  (define ex_sexp
    '(or (and tt ff)
         tt))
  (define ex
    (t->pict L (or (and tt ff)
                   tt))))

(module+ model2
  (require "../models/2-bool-state-everywhere.rkt")
  (provide lang schema new_ax bool_ax
           ex ex_sexp
           ex_st1 ex_st2
           ex_1 ex_2 ex_3 ex_4 ex_5 ex_6
           let_ax set_ax get_ax seq_ax)
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
                  10)))
  (define new_ax (jf->pict -->ax #:cases '("let" "set" "get" "seq")))
  (define bool_ax
    (list (jf->pict -->ax #:cases '("andtt" "andtf" "andft" "andff"))
          (jf->pict -->ax #:cases '("ortt" "ortf" "orft" "orff"))
          (jf->pict -->ax #:cases '("nott" "notf"))))

  (define let_ax (jf->pict -->ax #:cases '("let")))
  (define set_ax (jf->pict -->ax #:cases '("set")))
  (define get_ax (jf->pict -->ax #:cases '("get")))
  (define seq_ax (jf->pict -->ax #:cases '("seq")))

  (define ex_sexp
    '(let x
       (and x
            (seq (set x)
                 tt))))
  (define ex (t->pict L (let x
                           (and x
                                (seq (set x)
                                     tt)))))
  (define ex_1 (t->pict L (and x
                                (seq (set x)
                                     tt))))
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

  (define ec_lang (grammar->pict L #:nts '(E))))

(module+ model4
  (require "../models/4-shortcircuiting.rkt")
  (provide ax ec_lang red decomp)
  (define ec_lang (grammar->pict L #:nts '(E)))
  (define ax (list
              (jf->pict -->ax #:cases '("andt" "andf"))
              (jf->pict -->ax #:cases '("ort" "orf"))))
  (define red (jf->pict -->))
  (define decomp (t->pict L (in-hole E e))))
