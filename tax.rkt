;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname tax) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Task : Write a function to return the tax-due, given taxable
;  (for single tax-payers; we'll stop after 3 brackets)
;  ($9075 => 10%; $36900 => 15%; else 25%)
; A taxable income is:
;  A real number <= 9075
;  A real number in (9075,36900]
;  A real number in (36900,+++)

; template
(define (template-tax a-bracket)
  (cond [(<= income 9075) (* 0.10 income)]
        [(<= income 36900) (+ (* 0.10 income) (* 0.15 (- income 9075)))]
  )
)

(check-expect (tax 0) 0)
(check-expect (tax 9076)  ...)
(check-expect (tax 36900) ...)
(check-expect (tax -1) ...)
(check-expect (tax 2.05) ...)


; template taxable-income entry
; Examples of the data -- taxable-income-entry:
(define (function-for-tie a-tie)
  (cond [(symbol? a-tie)   ...]
        [(boolean? a-tie)  ...]
        [(<= a-tie 9075)   ...]
        [(<= a-tie 36900)  ...]
        [(else )           ...]
  )
)