;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname monogram) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; monogram : string string -> string
; Return the first and last initials
;   first-name, last-name - cannot be the empty string.

(define (monogram first-name last-name)
  (string-append
   (initialize first-name)
   (initialize last-name)
  )
)

; initialize : string -> string
; Return the first letter of a word downcased, following by ".".
;    word - cannot be the empty string
(define (initialize word)
  (string-append
   (string (char-downcase (string-ref word 0))) "."
  )
)

(check-expect (initialize "word") "w.")
(check-expect (initialize "Hello") "h.")
(check-expect (initialize "A") "a.")
(check-expect (initialize "#") "#.")
(check-expect (monogram "Ian" "Barland") "i.b.")
(check-expect (monogram "ian" "barland") "i.b.")
(check-expect (monogram "Jay" "Z") "j.z.")
(check-expect (monogram "!eric" "?huh") "!.?.")
(check-expect (monogram "Thug" "G") "t.g.")



