;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname natnum) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; a natnum is :
;   0, OR
;   (add1 [natnum])

; examples of the data

0                       ; 0
(add1 0)                ; 1
(add1 (add1 0))         ; 2
(add1 (add1 (add1 0)))  ; 3

; lol* : natnum -> string
; Return a string k times
(define (lol* k)
  (cond [(zero? k) ""]
        [(positive? k) (string-append "lol" (lol* (sub1 k)))]))

(check-expect (lol* 0) "")
(check-expect (lol* 1) "lol")
(check-expect (lol* 3) "lollollol")
(check-expect (lol* 7) "lollollollollollollol")

(require 2htdp/image)

; bullseye : natnum -> image
; Return a bullseye with n rings.
(define (bullseye n)
  (cond [(zero? n) (square 0 'solid 'orange)]
        [(positive? n) (overlay (bullseye (sub1 n))
                                (circle (* 10 n)
                                        'solid
                                        ; (list-ref cols (remainder n (length cols)))
                                        ; you cannot use lift-ref until you write it yourself
                                        (if (even? n) 'white 'blue)))]))

(check-expect (bullseye 3) (overlay (circle 10 'solid 'blue)
                                    (circle 20 'solid 'white)
                                    (circle 30 'solid 'blue)))

(check-expect (bullseye 2) (overlay (circle 10 'solid 'blue)
                                    (circle 20 'solid 'white)))

(check-expect (bullseye 1) (circle 10 'solid 'blue))

(check-expect (bullseye 0) (square 0 'solid 'orange))

