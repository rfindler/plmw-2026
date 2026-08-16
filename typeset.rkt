#lang racket
(require racket/gui/base redex/pict pict)
(provide jf->pict grammar->pict t->pict str_to_tt)

(define font-size 32)

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

(define (-->ax1-rewrite lws)
  (match lws
    [(list _open _--> e1 e2 _close)
     (generic-arrow-rewrite (list e1) (list e2) "ax" #f)]))
(define (-->ax2-rewrite lws)
  (match lws
    [(list _open _--> Σ1 e1 Σ2 e2 _close)
     (generic-arrow-rewrite (list Σ1 e1) (list Σ2 e2) "ax" #f)]))

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
      (['ax-->1 -->ax1-rewrite]
       ['ax-->2 -->ax2-rewrite]
       ['ax-->3 -->ax2-rewrite]
       ['extend extend-rewrite]
       ['lookup lookup-rewrite]
       ['⊕ ⊕-rewrite])
    (parameterize ([label-font-size font-size]
                   [default-font-size font-size]
                   [metafunction-font-size font-size]
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
