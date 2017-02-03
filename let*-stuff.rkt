;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname let*-stuff) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define a 17)
(define msg "hello")

; let and let* are similar syntax -> different semantics
; let will evaluate the right side first then set that equal to the left identifiers
; let* is normally the one people use

(let* {[msg "aloha"]
       [state "hawai'i"]
       [b (+ a 3)]
       [c (* a b)]}
  (string-append msg " " (number->string a)))

; list of stuff using cons and let statements
(let {[msg "aloha"]}
  (let {[b (+ a 3)]}
    (let {[c (* b 7)]}
      (cons c (cons b (cons msg (cons a empty)))))))

; Function for quadratic formula
(define (root a b c)
  (let* {[discriminant (- (* b b) (* 4 a c))]
         [numerator (+ (- b) (sqrt discriminant))]
         [denominator (* 2 a)]}
    (/ numerator denominator)))

(root 1 2 1)

; syntactic sugar : rewriting one bit of syntax as another, more-primative one