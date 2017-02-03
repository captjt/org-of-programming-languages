#lang racket
; Jordan Taylore

;;; Search for ">>>Q3" or ">>>Q4", for changes made since Q0.

; We'll run this program in language-level 'Racket', *not* 'Advanced Student'.
; (This is because we are exporting our functions
;  as a module, so the test-cases can use them.)

; This next line exports all functions
; (in Java terms: a package where everything is public).
; Alternately, you could `provide` just the specific names you wanted public.
;
(provide (all-defined-out))

(require "scanner.rkt")  ; scanner.rkt must be in the same dir as this file.

;;;;;;;;;;; language Q4 ;;;;;;;;;;;;;;

; An Expr is one of:
; - a number
; - (make-paren-expr [Expr])
; - (make-bin-expr [Expr] [Bin-op] [Expr])
; - (make-if-zero-expr [Expr] [Expr] [Expr])   ;>>>Q1
; - (make-parity-expr [Expr] [Expr] [Expr])
; - string                                   ;>>>Q2
; - (make-let-expr   [String] [Expr] [Expr]) ;>>>Q2
; - (make-func-expr [String] [Expr])     ;>>>Q4
; - (make-func-apply-expr [Expr] [Expr]) ;>>>Q4
; A Bin-op is a string in the list BIN_OP_TOKENS:
;
(define BIN_OP_TOKENS (list "add" "sub" "mul" "mod"))

(define (bin-op? any-val) (member any-val BIN_OP_TOKENS))


(define-struct bin-expr (left op right closure) #:transparent)
(define-struct paren-expr (e closure) #:transparent)
(define-struct if-zero-expr (cond then else closure) #:transparent)
(define-struct parity-expr (cond even odd closure) #:transparent)
(define-struct let-expr   (id rhs body closure) #:transparent)   ;>>>Q2
(define-struct func-expr  (param body closure) #:transparent) ;>>>Q4
(define-struct func-apply-expr (f arg) #:transparent) ;>>>Q4

;
; The keyword 'transparent' makes structs with 'public' fields;
; in particular check-expect can inspect these structs.

#| Examples of the data:
   8                                 (written as "8")
   (make-paren-expr 8)               (written as "[[8]]")
   (make-bin-expr 8 "add" 4)         (written as "( 8 add 4 )")
   (make-bin-expr (make-bin-expr 8
                                 "add"
                                 4)
                  "mul"
                  5)  
                                      (written as "( ( 8 add 4 ) mul 5 )"
  ; See expr-test-Q4 for examples of func-expr, func-apply-expr.
|#
(define hash-sig #x814F8614)

; assert-pop! : string string -> (void)
; Pop a token off `s`; raise an exception if it's not `required-token`.
;
(define (assert-pop! s required-token)
  (let* {[token (pop! s)]}
    (unless (equal? token required-token)
      (error 'assert-pop! "Expected ~v, but got ~v." required-token token))))

       
; parse!: ( scanner -> expr)
; Return (our internal, parse-tree representation of) the first Q0 expr
; at the front of the scanner.
; Side effect: Consumes tokens from scnr corresponding exactly
; to the returned expr.
;
; "recursive descent parsing"
;
(define (parse! scnr)
  (cond [(string? scnr) (parse! (create-scanner scnr))]
        ; The above cond-branch is overloading to accept a string *or* a scanner.
        ; The following cond-branches correspond directly to the grammar.
        [(number? (peek scnr)) (pop! scnr)]
        [(string=? (peek scnr) "if")
         (let* {[open-if-zero (pop! scnr)]  ; Pop off the "if" we just peeked at.  Ignore it.
                [subexpr1 (parse! scnr)]
                [the-is-keyword  (pop! scnr)] ; pop off the "is";  ignore it.
                [the-zero-keyword  (pop! scnr)] ; pop off the "zero";  ignore it.
                [the-then-keyword  (pop! scnr)] ; pop off the "then";  ignore it.
                [subexpr2 (parse! scnr)]
                [the-else-keyword (pop! scnr)] ; pop off the "else"; ignore it.
                [subexpr3 (parse! scnr)]
                [the-at (pop! scnr)] ; pop off the "@"; ignore it.
                }
            (make-if-zero-expr subexpr1 subexpr2 subexpr3))]
        [(string=? (peek scnr) "parity") ;>>>Q1: parity
         (let* {[_ (pop! scnr)]  ; Pop off the "parity" we just peeked at.  Ignore it.
                [subexpr1 (parse! scnr)]
                [_  (pop! scnr)] ; pop off the "even:";  ignore it.
                [subexpr2 (parse! scnr)]
                [_ (pop! scnr)] ; pop off the "odd:"; ignore it.
                [subexpr3 (parse! scnr)]
                [_ (pop! scnr)] ; pop off the ";"; ignore it.
                }
           (make-parity-expr subexpr1 subexpr2 subexpr3))]
        [(string=? (peek scnr) "[")
         (let* {[open-square1 (pop! scnr)]
                [open-square2 (pop! scnr)]
                [the-inside-expr (parse! scnr)]
                [close-square1 (pop! scnr)]
                [close-square2 (pop! scnr)]
                }
           (make-paren-expr the-inside-expr))]
        [(string=? (peek scnr) "(")
         (let* {[_ (assert-pop! scnr "(")]
                [id-or-left (parse! scnr)] ;>>>Q4 we don't know yet: bin-expr's left, or func-expr's id?
                [close-or-op (pop! scnr)]  ;>>>Q4 we don't know yet: bin-expr's binop, or func-expr's ')'?
                }
            (if (string=? ")" close-or-op) ;>>>Q4
                (let* {  ; Oh, we have a func-expr -- parse the rest of it.  ;>>>Q4
                       [_ (assert-pop! scnr "-")]
                       [_ (assert-pop! scnr ">")]
                       [_ (assert-pop! scnr "{")]
                       [the-body (parse! scnr)]
                       [_ (assert-pop! scnr "}")]
                       }
                  (make-func-expr id-or-left the-body))
                (let* { ; Oh, we have a bin-expr -- parse the rest of it.
                       [subexpr2 (parse! scnr)]
                       [_ (assert-pop! scnr ")")]
                       }
                   (when (not (bin-op? close-or-op))
                         (error 'parse! "Expected one of ~v, got ~v." BIN_OP_TOKENS close-or-op))
                   (make-bin-expr id-or-left close-or-op subexpr2))))]

         [(string=? (peek scnr) "say") ;>>>Q2: LetExpr
          (let* {[_ (pop! scnr)]  ; Pop off the "say" we just peeked at.  Ignore it.
                 [the-id (pop! scnr)]
                 [_  (pop! scnr)] ; pop off the "be";  ignore it.
                 [the-rhs (parse! scnr)]
                 [_ (pop! scnr)] ; pop off the "in"; ignore it.
                 [the-body (parse! scnr)]
                 [_ (pop! scnr)] ; pop off the "matey"; ignore it.
                }
           (make-let-expr the-id the-rhs the-body))]
         [(string=? (peek scnr) "<")   ;>>>Q4 -- func-apply-expr
          (let* {[_ (pop! scnr)] ; "["
                 [f (parse! scnr)]
                 [_ (pop! scnr)] ; "@"
                 [arg (parse! scnr)]
                 [_ (pop! scnr)] ; ">"
                 }
            (make-func-apply-expr f arg))]
         [(string? (peek scnr)) (pop! scnr)] ;>>>Q2: ID
         [else (error 'parse! "Unrecognized expr: ~v." (peek scnr))]))

; string->expr (-> string expr)
; Convert the given string representing exactly *one* single Expr
; into our internal format (structs).
;
(define (string->expr str)
  (parse! (create-scanner str)))



; expr->string : (-> expr string) (a.k.a. toString)
; Return a human-readable version of our internal representaiton of Exprs.
; 
(define (expr->string e)
  (cond [(number? e) (format "~v" e)]
        [(paren-expr? e) (string-append
                          "[["
                          (expr->string (paren-expr-e e))
                          "]]")]
        [(bin-expr? e) (string-append
                        "(" 
                        (expr->string (bin-expr-left e))
                        " "
                        (bin-expr-op e)
                        " "
                        (expr->string (bin-expr-right e))
                        ")")]
        [(if-zero-expr? e) (string-append  ;>>>Q1: 
                            "if "
                            (expr->string (if-zero-expr-cond e))
                            " is zero then "
                            (expr->string (if-zero-expr-then e))
                            " else "
                            (expr->string (if-zero-expr-else e))
                            "@")]
        [(parity-expr? e) (string-append
                            "parity "
                            (expr->string (parity-expr-cond e))
                            " even: "
                            (expr->string (parity-expr-even e))
                            " odd: "
                            (expr->string (parity-expr-odd  e))
                            ";")]
        [(string? e) e] ;>>>Q2: ID
        [(let-expr? e) (string-append ;>>>Q2
                          "say "
                          (let-expr-id e)
                          " be "
                          (expr->string (let-expr-rhs e))
                          " in "
                          (expr->string (let-expr-body e))
                          " matey")]
        [(func-expr? e) (string-append ;>>>Q4
                         "("
                         (func-expr-param e)
                         ") -> {"
                         (expr->string (func-expr-body e))
                         "}")]
        [(func-apply-expr? e) (string-append ;>>>Q4
                               "<"
                               (expr->string (func-apply-expr-f e))
                               "@"
                               (expr->string (func-apply-expr-arg e))
                               ">")]
        
        [else (error 'expr->string "unknown internal format?;: ~v" e)]
        ))




; eval : (-> expr val)
; Evaluate the given expr.
;
(define (eval e)
  (cond [(number? e) e]
        [(paren-expr? e) (eval (paren-expr-e e))]
        [(bin-expr? e) (eval-bin-expr e)] ; defer to a helper
        [(if-zero-expr? e) ;>>>Q1
         (if (= (eval (if-zero-expr-cond e)) 0)
             (eval (if-zero-expr-then e))
             (eval (if-zero-expr-else e)))]
        [(parity-expr? e)
         (if (even? (eval (parity-expr-cond e)))
             (eval (parity-expr-even e))
             (eval (parity-expr-odd  e)))]
        [(let-expr? e) ;>>>Q2
         (let* {[v (eval (let-expr-rhs e))]
                [e′ (subst (let-expr-id e)
                           v
                           (let-expr-body e))]}
            (eval e′))]
        [(string? e) (error 'eval "Undeclared identifier: ~v" e)] ;>>>Q2
        [(func-expr? e) e] ;>>>Q4
        [(func-apply-expr? e) (let* {[f (eval (func-apply-expr-f e))]       ;>>>Q4
                                     [arg (eval (func-apply-expr-arg e))]
                                     }
                                (eval (subst (func-expr-param f) arg (func-expr-body f))))]
        [else (error 'eval "unknown internal format?;: ~v" e)]))

; eval-q5 : (-> function val)
;>>>>>Q5

; eval-bin-expr : (-> bin-expr val)
;
(define (eval-bin-expr e)
  (let* {[left-value  (eval (bin-expr-left  e))]
         [right-value (eval (bin-expr-right e))]
         }
    (cond [(string=? (bin-expr-op e) "add")  (+ left-value right-value)]
          [(string=? (bin-expr-op e) "mul") (* left-value right-value)]
          [(string=? (bin-expr-op e) "sub") (- left-value right-value)]
          [(string=? (bin-expr-op e) "mod") ;>>>Q1
           (- left-value (* right-value (floor (/ left-value right-value))))]
          [else (error 'eval-bin-expr
                       "Syntax error: unknown binary operator '~a' in ~a."
                       (bin-expr-op e)
                       (expr->string e))])))


; subst : string number expr -> expr
; Return `e` but with any occurrences of `id` replaced with `v`.
;
(define (subst id v e)
  (cond [(number? e) e]
        [(paren-expr? e) (make-paren-expr (subst id v (paren-expr-e e)))]
        [(bin-expr? e) (make-bin-expr (subst id v (bin-expr-left e))
                                      (bin-expr-op e)
                                      (subst id v (bin-expr-right e)))]
        [(if-zero-expr? e) (make-if-zero-expr (subst id v (if-zero-expr-cond e))  ;>>>Q1
                                              (subst id v (if-zero-expr-then e))
                                              (subst id v (if-zero-expr-else e)))]
        [(parity-expr? e) (make-parity-expr (subst id v (parity-expr-cond e))
                                            (subst id v (parity-expr-even e))
                                            (subst id v (parity-expr-odd  e)))]
        [(let-expr? e) (make-let-expr (let-expr-id e) ;>>>Q3 -- don't ever subst a binding occurrence
                                      ; We know the above is an id, not *any* Expr.
                                      (subst id v (let-expr-rhs e))
                                      (if (string=? id (let-expr-id e))  ;>>>Q3: if new var =id, we're shadowing
                                          (let-expr-body e)              ;>>>Q3: so don't subst the bound occur's
                                          (subst id v (let-expr-body e))))]
        [(func-expr? e) (make-func-expr (func-expr-param e)                    ;>>>Q4: func-exprs, like let-exprs,
                                        (if (string=? id (func-expr-param e))  ;>>> introduce bindings, so shadow if needed.
                                            (func-expr-body e)
                                            (subst id v (func-expr-body e))))]
        [(func-apply-expr? e) (make-func-apply-expr (subst id v (func-apply-expr-f e))
                                                    (subst id v (func-apply-expr-arg e)))]
        [(string? e) (if (string=? e id) v e)]
        [else (error 'expr->string "unknown internal format?;: ~v" e)]
        ))


; >>>> Q5

; SCOPE Extra Credit

(define a 10)
(define b 20)
(define make-foo
  (let {[b 30]}
    (lambda ()            ; ← make-foo is bound to this function.
      (let {[c 40]}
        (lambda (cmd)     ; ← make-foo returns this function as its answer.
          (let {[d 50]}
            (cond [(symbol=? cmd 'incA) (set! a (+ a 1))]
                  [(symbol=? cmd 'incB) (set! b (+ b 1))]
                  [(symbol=? cmd 'incC) (set! c (+ c 1))]
                  [(symbol=? cmd 'incD) (set! d (+ d 1))]
                  [(symbol=? cmd 'get-all) (list a b c d)])))))))
#|
The scope of the a declared in line 314 is lines 314 through 314.

The scope of the b declared in line 315 is lines 315 through 315.

The scope of the b declared in line 317 is lines 317 through 326.

The scope of the c declared in line 319 is lines 319 through 326.

The scope of the d declared in line 321 is lines 321 through 326.
Suppose that make-foo is called exactly three times (but that a function returned by make-foo is not called).

How many variables named a are created? 0

How many variables named b are created? 6                                                                                                                                                                                            

How many variables named c are created? 6    

How many variables named d are created? 6

|#
