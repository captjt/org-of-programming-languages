;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname list-of-nums) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; data definition
; a list-of-numbers is
;  - '(), OR
;  - (cons [number] [list-of-numbers])

; Template function
; func-for-list : list-of-num -> ??
;
; (define (func-for-list a-lon)	
;   (cond [(empty? a-lon) ...]
;         [(cons? a-lon) (... first a-lon)
;                         ... (func-for-list (rest a-lon))]))

; contains-93? : list-of-number -> boolean
; return whether 'a-lon' contains 93.
;
(define (contains-93? a-lon)
  (cond [(empty? a-lon) false]
        [(cons? a-lon) (or (= 93 (first a-lon))
                           (contains-93? (rest a-lon)))]))

; another way to write it

; (define (contains-93? a-lon)
;   (cond [(empty? a-lon) false]
;         [(cons? a-lon) (if (= 93 (first a-lon))
;                            #t
;                            (contains-93? (rest a-lon)))]))

(check-expect (contains-93? '()) #f)
(check-expect (contains-93? (cons 93 '())) #t)
(check-expect (contains-93? (cons 17 '())) #f)
(check-expect (contains-93? (cons -2 (cons 93 '()))) #t)
(check-expect (contains-93? (cons 93 (cons 17 '()))) #t)
(check-expect (contains-93? (cons -2 (cons 17 '()))) #f)
(check-expect (contains-93? (cons 93 (cons 93 '()))) #t)

; triple-every-even (and leave the odd number untouched)
;
(define (tee a-lon)
  (cond [(empty? a-lon) '()]
        [(cons? a-lon) (cons (if (even? (first a-lon))
                                (* 3 (first a-lon))
                                (* 1 (first a-lon)))
                            (tee (rest a-lon)))]))

(check-expect (tee '()) '())
(check-expect (tee (cons 4 '())) (cons 12 '()))
(check-expect (tee (cons 7 '())) (cons 7 '()))


(define (my-max a-lon)
  (cond [(empty? a-lon) -inf.0]
        [(cons? a-lon)
         (let* {[max-of-rest (my-max (rest a-lon))]
                [legendres-constant 1]
                [... ...]
                }
           (if ... ) 
             