;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hw03b) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; homework 03b
; Jordan Taylor
; ITEC 380 - Barland
; Due : September 21, 2015

(require 2htdp/image)

; Data definition
; A directory response is
;   a username -> String
;   #f         -> Boolean
;   'private   -> Boolean
; email-response : a-directory-response -> string
; Return a string giving an email address, no entry in directory,
;    or user is private

(define (email-response a-directory-response)
  (cond [(symbol? a-directory-response) "Information not published"]
        [(string? a-directory-response) (string-append a-directory-response "@radford.edu")]
        [(boolean? a-directory-response) "No such person"] 
    ) 
  )

(check-expect (email-response "jtaylor32") "jtaylor32@radford.edu")
(check-expect (email-response "") "@radford.edu")
(check-expect (email-response #f) "No such person")
(check-expect (email-response 'private) "Information not published")
(check-expect (email-response "1") "1@radford.edu")


; make-donut : non-negative real number -> image
; return a brown donut image when given a certain non-negative real number

(define (make-donut a-number)
  (underlay (circle a-number 'solid 'brown)
            (circle (/ a-number 2) 'solid 'white)
    )
  )

(check-expect (make-donut 32) (underlay (circle 32 'solid 'brown)
            (circle (/ 32 2) 'solid 'white)
    )
  )
(check-expect (make-donut 16) (underlay (circle 16 'solid 'brown)
            (circle (/ 16 2) 'solid 'white)
    )
  )
(check-expect (make-donut 0) (underlay (circle 0 'solid 'brown)
            (circle (/ 0 2) 'solid 'white)
    )
  )

; make-eyes : non-negative real number, a color -> image
; return a set of circles next to each other that look like eyes

(define (make-eyes a-number a-color)
  (beside (make-circles a-number a-color)
          (make-circles a-number a-color) 
    )
  )

(define (make-circles a-number a-color)
  (underlay (circle a-number 'solid a-color)
            (circle (/ a-number 2) 'solid 'white)
    )
  )

(check-expect (make-eyes 12 'green)
   (beside (underlay (circle 12 'solid 'green)
            (circle (/ 12 2) 'solid 'white)
          )
          (underlay (circle 12 'solid 'green)
            (circle (/ 12 2) 'solid 'white)
          )
    )
)
(check-expect (make-eyes 32 'green)
   (beside (underlay (circle 32 'solid 'green)
            (circle (/ 32 2) 'solid 'white)
          )
          (underlay (circle 32 'solid 'green)
            (circle (/ 32 2) 'solid 'white)
          )
    )
)