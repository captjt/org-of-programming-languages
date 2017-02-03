;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname expr-test-Q4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))
; expr-test-Q4, test cases for Q4
; jtaylor32 , 2015-Dec-08
; see http://www.radford.edu/~itec380/2015fall-ibarland/Homeworks/hw08.html
(require "Q4.rkt")
(require "scanner.rkt")
(require rackunit) ; use `check-equal?`, not `check-expect`.
(require "student-extras.rkt")
;
; We use `check-equal?` because it can be passed as a function (e.g. to `map`).
; The one advantage of `check-expect` we lose, is that all checks are delayed
; until the end of the file (hence, you can have a `check-expect` *before*
; the function it tests -- not so, with `check-equal?`.)



;;;;;;;;;;;;;;;;;;; TEST CASES: Q0-Q4 ;;;;;;;;;;;;;;;;


; Some expressions to test in a non-automated way:
(define e0 "43")
(define e1 "[[43]]")
(define e2 "(4 add 3)")
(define e3 "[[[[[[(4 add 3)]]]]]]")
(define e4 "([[43]] add    (  42 mul 3))")
(define e5 "zz")
(define e6 "say zz be 5 in ( zz add 9 ) matey")

; examples from hw08 -- #3:
(define 3a "say y be 3 in say x be 5 in (x add y) matey matey")
(define 3b "say y be 3 in say x be y in (x add y) matey matey")
(define 3c "say x be 5 in say y be 3 in (say x be y in (x add y) matey add x)matey matey")
(define 3d "say x
             be 5
             in [[say x
                   be (x add 1)
                   in (x add 2) matey]] matey")
(define 3e "say y
             be say z
                 be 4
                 in [[say y
                       be 99 
                       in z matey]]matey
             in [[say z
                   be 5
                   in ([[say z
                          be 10
                          in y matey]]
                       add
                       (y add z)) matey]] matey")

; subst for 3a:
(check-equal? (subst "y" 3 (string->expr "say x be 5 in ( x add y ) matey"))
              (string->expr "say x be 5 in (x add 3) matey"))

; subst for 3b:
(check-equal? (subst "y" 3 (string->expr "say x be y in (x add y) matey"))
              (string->expr "say x be 3 in (x add 3) matey"))

; subst for 3c:
(check-equal? (subst "x" 5 (string->expr "say y be 3 in (say x be y in (x add y) matey add x)matey"))
                           (string->expr "say y be 3 in (say x be y in (x add y) matey add 5)matey"))

; subst for 3d:
(check-equal? (subst "x" 5 (string->expr "[[say x be
                                                  (x add 1) in
                                                  (x add 2) matey]]"))
              (string->expr "[[say x be
                                     (5 add 1) in
                                     (x add 2) matey]]"))

; subst for 3e:
(check-equal? (subst "y"
                     (eval (string->expr "say z
                                           be 4
                                           in [[say y
                                                 be 99
                                                 in z matey]]matey"))
                     (string->expr "[[say z  
                                       be 5  
                                       in ([[say z
                                              be 10
                                              in y matey]]
                                            add
                                            (y add z)) matey]]"))
              (string->expr "[[say z  
                                be 5  
                                in ([[say z
                                       be 10
                                       in 4 matey]]
                                     add
                                     (4 add z)) matey]]"))

(check-equal? (eval (string->expr 3a))  8)
(check-equal? (eval (string->expr 3b))  6)
(check-equal? (eval (string->expr 3c)) 11)
(check-equal? (eval (string->expr 3d))  8)
(check-equal? (eval (string->expr 3e)) 13)


;>>> Q4
(define seventeen.q4 "(x) -> { 17 }")
(define tio.q4 "(x) -> { parity x even: (x sub 1) odd: ((x mul 3) add 1);}")
(define sqr.q4 "(x) -> {(x mul x)}")
(define make-adder.q4 "(n) -> {(m) -> {(n add m)}}")


(check-equal? (expr->string "zz") "zz")
(check-equal? (string->expr "zz") "zz")
(check-exn    exn:fail? (λ() (eval "zz")))

(check-equal? (string->expr "39") 39)
(check-equal? (eval (string->expr e4)) 169)

;;; three types of test we want to make, for many different exprs:
(check-equal? (string->expr "(4 add 3)") (make-bin-expr 4 "add" 3))
(check-equal? (eval (string->expr "(4 add 3)")) 7)
(check-equal? (expr->string (string->expr "(4 add 3)"))
              "(4 add 3)")






;; If you wanted to test/verify that exceptions really do get thrown
;; on particular inputs, you can use `check-exn`.
;;
(check-exn exn:fail? (λ() (string->expr "[[]]")))
;>>>Q2:
(check-exn #rx"Undeclared identifier" (λ() (eval "x")))
;;
;; You don't need to do this -- I'm just including it to show
;; what things a unit-testing library can/should check for.
;;
;; Btw, I should call `(regexp (regexp-quote ...the-err-message...))`
;;   or at least quote the "." in my #rx above.
;; Althougy regexp-quote isn't in the student languages,
;; you can `(require "student-extras.rkt")` to get it.


(define tests
  ; Each entry in the list is either
  ; [str val] (where val is the result of interpreting the string str), or
  ; [str val expr] (as above, but expr is the internal (struct) representation).
  `{["7" 7 7]
    ["(3 add 4)" 7 ,(make-bin-expr 3 "add" 4)]
    ["(3 mul   4)" 12 ,(make-bin-expr 3 "mul" 4)]
    ["[[((3 add 4) add(  3  mul 4 ) )]]" 19]
    ["parity  0 even: 1 odd: 2;" 1]
    ["parity -8 even: 1 odd: 2;" 1]
    ["parity  7 even: 1 odd: 2;" 2]
    ["( 4 add [[parity 8 even: [[parity 7 even: 1 odd: 2;]] odd: 3;]] )" 6]
    
    #| Further tests, for ;>>>Q1: |#
    ["(3.0 mod 4.0)" 3]
    ["(( 5.0 add 6.0 ) mod 3.0)" 2]
    ["(8.1 mod 3.0)" 2.1]
    ["(8.0 mod 3.1)" 1.8]
    ["(-8.1 mod 3.0)" 0.9]
    ["(-8.0 mod 3.1)" 1.3]
    ["(8.1 mod -3)" -0.9]
    ["(8.0 mod -3.1)" -1.3]
    ["(-8.1 mod -3.0)" -2.1]
    ["(-8.0 mod -3.1)" -1.8]
    ["(8.0  mod  2.0)" 0]
    ["(-8.0  mod 2.0)" 0]
    ["(8.0 mod  -2.0)" 0]
    ["(-8.0 mod -2.0)" 0]
    ["(8.0  mod  3.0)" 2]
    ["(-8.0  mod 3.0)" 1]
    ["(8.0 mod  -3.0)" -1]
    ["(-8.0 mod -3.0)" -2]
    
    ;>>>Q1
    ["if 0 is zero then 1 else 2@" 1 ,(make-if-zero-expr 0 1 2)]
    ["if 1 is zero then 1 else 2 @" 2 ,(make-if-zero-expr 1 1 2)]
    ["if (3 add -3) is zero then 1 else 2@" 1 ,(make-if-zero-expr (make-bin-expr 3 "add" -3) 1 2)]
    ["if (if if 0 is zero then 1 else 2@ is zero then 3 else 4 @ add -3) is zero then  1 else 2@" 
     2
     ,(make-if-zero-expr (make-bin-expr (make-if-zero-expr (make-if-zero-expr 0 1 2) 3 4) "add" -3) 1 2)]
    
    ;>>>Q2
    ["say x be 5 in 7 matey" 7 ,(make-let-expr "x" 5 7)]
    ["say x be 5 in x matey" 5 ,(make-let-expr "x" 5 "x")]
    ["say x be 5 in (4 mul x) matey" 20 ,(make-let-expr "x" 5 (make-bin-expr 4 "mul" "x"))]
    ; and a test where the say isn't top-level:
    ["( [[say x be 5 in (4 mul x) matey]] add 2 )" 22]

    ;>>>Q4
    [,seventeen.q4
     ,(make-func-expr "x" 17)
     ,(make-func-expr "x" 17)]

    [,tio.q4
     ,(make-func-expr "x" (make-parity-expr "x" (make-bin-expr "x" "sub" 1) (make-bin-expr (make-bin-expr "x" "mul" 3) "add" 1)))
     ,(make-func-expr "x" (make-parity-expr "x" (make-bin-expr "x" "sub" 1) (make-bin-expr (make-bin-expr "x" "mul" 3) "add" 1)))]

    ["<(x) -> { 17 } @ 3>" 17 ,(make-func-apply-expr (make-func-expr "x" 17) 3)]
    [,(string-append "<" tio.q4 "@ +3>") 10]
    [,(string-append "<" tio.q4 "@ +2>")  1]
    [,(string-append "<" sqr.q4 "@ -3>") 9]
    [,(string-append "<" sqr.q4 "@ -3>") 9]
    [,(string-append "<" make-adder.q4 "@ 2>")
     ,(make-func-expr "m" (make-bin-expr 2 "add" "m"))
     ,(make-func-apply-expr (make-func-expr "n" (make-func-expr "m" (make-bin-expr "n" "add" "m"))) 2)]
    [,(string-append "<<" make-adder.q4 "@ 2> @ 7>") 9]
  
    })
; For info on backquote, see documentations and/or:
;   http://www.radford.edu/itec380/2014fall-ibarland/Lectures/backquote.html


; The last paragraph of #2 on hw07 mentions that you'll have to do substitution in a tree.
; Although `substitute` returns a *tree* (an Expr), 
; we can use `parse` (a.k.a. string->expr) (already tested;) to help us generate our expected-results.
;
;>>>Q2:
(check-equal? (subst "x" 9 (string->expr "3"))   (string->expr "3") )
(check-equal? (subst "x" 9 (string->expr "x"))   (string->expr "9") )
(check-equal? (subst "z" 7 (string->expr "x"))   (string->expr "x") )
(check-equal? (subst "z" 7 (string->expr "( 4 add z )"))   (string->expr "( 4 add 7 )"))
(check-equal? (subst "z" 7 (string->expr "say x be z in ( x mul z ) matey"))  
              (string->expr "say x be 7 in ( x mul 7 ) matey"))
; Give at least one more interesting tree, to test `substitute` on,
; with parse-tree of height of 2 or more.
; You do *not* need to do `substitute` on a parse tree containing a `let` inside of it ... yet.
; (But you are encouraged to start thinking about what you want to happen, in that situation.)

(check-equal? (subst "x" 9 (string->expr "(x add ( 3 sub [[[[x]]]] ))"))
              (string->expr "(9 add ( 3 sub [[[[9]]]] ))"))
(check-equal? (subst "x" 9 (string->expr "(x add [[parity x even: (x add 2) odd: [[(x sub x)]];]])"))
              (string->expr "(9 add [[parity 9 even: (9 add 2) odd: [[(9 sub 9)]];]])"))





;;;;;;;;;;;;;;;;;;;; test harness functions, below ;;;;;;;;;;;;;;;;
; Given a string, return a list of tokens (of Q).
;
(define (string->Q-tokens str)
  ; Don't use scheme's built-in `read`, because our string might contain semicolons etc.
  (let loop {[scnr (create-scanner str)]} ; See "named let" -- advanced-student.
    (if (eof-object? (peek scnr))
        empty
        (cons (pop! scnr) (loop scnr)))))
        ; N.B. We RELY on left-to-right eval here:
        ; the pop happens before looping back.

(define hash-sig-test #x814F8614)

; Test the internal representations:
(for-each (λ (t) (check-equal? (string->expr (first t))
                               (third t)))
          (filter (λ(t) (>= (length t) 3)) tests))



; Test that expr->string and string->expr are inverses:
(for-each (λ (t) (check-equal? (string->Q-tokens (expr->string (string->expr (first t))))
                              (string->Q-tokens (first t))))
          tests)


; Now, test `eval`:
(for-each (λ (t) (check-equal? (eval (string->expr (first t)))
                              (second t)))
          tests)
