;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname loops) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; There isn't a 'while' loop built in.
; Can we write our own, as a function?
; How would we call it?

(check-expect (nums-down-from 7)
              (list 7 6 5 4 3 2 1))
;(sqrts-down-from 17) = (list 4.101 4 3.98 ... 1.414 1)

(define (nums-down-from n)
  (while/list n positive? sub1))

(check-expect (while/list (list "a" "b" "c") cons? rest)
              (list  (list "a" "b" "c")
                     (list "b" "c")
                     (list "c"))) 

; while/list:  α, (α -> boolean), (α -> α)  ->  (list-of α)
; Return a list containing `indx`, `(update indx)`, `(update (update indx))`, ...
; so long as `continue?` of that quantity returns true.

(define (while/list indx continue? update)
  (reverse (wl-help indx continue? update empty)))

; wl-help : α, (α -> boolean), (α -> α), (list-of α)  ->  (list-of α)
; Return a list containing `indx`, `(update indx)`, `(update (update indx))`, ...
; so long as `continue?` of that quantity returns true --
; all prepended to the front of `results-so-far`.

(define (wl-help indx continue? update result-so-far)
  (cond [(continue? indx) (wl-help (update indx)
                                   continue?
                                   update
                                   (cons indx result-so-far))]
        [else result-so-far]))

; Q1: Call while/list to count from 10 to (but not including) 50, by 7s.

(while/list 10
            (λ (k) (< k 50))
            (λ (n) (+ n 7)))

; Q2: Write the function 'nums-from-to',
;     whose body is just a single call to `while/list`.
;
; Q3: Write `for/list`, which takes a start, stop, and body
;     and returns a list of all the body-results.
;      i. Implement this w/o calling any of the functions above
;     ii. What's the cheapo way of writing this, using functions above?

;;;;
(require 2htdp/image)

(while/list empty-image
            (λ (img) (< (image-width img) 150))
            (λ (img) (let* {[r (image-width img)]}
                       (overlay img
                                (square (+ r 9)
                                        'solid
                                        (if (even? r)
                                            'blue
                                            'red))))))

