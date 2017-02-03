;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname mapping) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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


; "mapping" a function means: apply the function to each element of
;    getting back a list of the results

(define strs0 '())
(define strs1 (cons "hello" '()))
(define strs2 (cons "Guten Tag" strs1))
(define strs3 (cons "ciao" strs2))

; map-string-length : (list-of string) -> (list-of natnum)
; map 'string-length' to 'strs'
(define (map-string-length strs)
  (cond [(empty? strs) '()]
        [(cons? strs) (cons (string-length (first strs))
                            (map-string-length (rest strs)))]))

(check-expect (map-string-length strs3)
              (cons 4 (cons 9 (cons 5 '()))))

; map-initialize : (list-of string) -> (list-of initialzed string)
; map 'initialize' to 'strs'
#;(define (map-initialize strs)
    (cond [(empty? strs) '()]
          [(cons? strs) (cons (initialize (first strs))
                              (map-initialize (rest strs)))]))

(define (map-initialize strs)
  (map initialize strs))

(check-expect (map-initialize strs3)
              (cons "c." (cons "g." (cons "h." '()))))


; my-map : (alpha -> beta), list-of-alpha -> list-of-beta
;    given a list of 'list-things', return a list of
;    the results of applying 'function' to each item in 'list-things'
#;(define (my-map f list-things)
  (cond [(empty? strs) '()]
        [(cons? strs) (cons (f (first list-things))
                            (my-map f (rest list-things)))]))

; map-string-upcase : (list-of string) -> (list-of upcased string)
(define (map-string-upcase strs)
  (map (λ (s) (list->string (map char-upcase (string->list s))))
       strs))

; use 'char-upcase', 'string-list', and 'list-string'

(check-expect (map-string-upcase strs3)
              (cons "CIAO" (cons "GUTEN TAG" (cons "HELLO" '()))))

(check-expect (string-append-whoa empty) "whoa")
(check-expect (string-append-whoa (list "aloha")) "aloha whoa")
(check-expect (string-append-whoa (list "bye" "aloha")) "bye aloha whoa")
(check-expect (string-append-whoa (list "hi" "bye" "aloha")) "hi bye aloha whoa")

; 'string-append-whoa' function
; 