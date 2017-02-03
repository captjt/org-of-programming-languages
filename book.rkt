;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname book) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Data definition
; A book is
;   (make-book [string] [string] [natural] [boolean])

(define-struct book (title author num-pages is-copyrighted?))

(make-book "The Cat in the Hat" "Suess" 37 #true)
(define b1 (make-book "Hello World" "Hitchens" 1 #false))

; 'define-struct' actually introduces additional functions:
; a constructor
; and four getters ("selectors").
; In this case, the constructor is named
;   make-book : string, string, natural, boolean ->
; and the getters are
;   book-title : book -> string
;   book-author : book -> string
;   book-num-pages : book -> natural
;   book-is-copyrighted? : book -> boolean

(book-title (make-book "The Cat in the Hat" "Suess" 37 #true))

; reading time : book -> non-negative real 
; return the amount of time it takes the average reader to read a book (in minutes)
; 

(define (reading-time a-book)
  (* (book-num-pages a-book) 2)
  )

(check-expect (reading-time (make-book "The Cat in the Hat" "Suess" 37 #true)
              ) 74)
(check-expect (reading-time b1) 2)

; book->string : book -> string
; return a string-representation of a book struct

(check-expect (reading-time (make-book "The Cat in the Hat" "Suess" 37 #true)
              ) "The Cat in the Hat, by Suess (37pp), ")

 
(define (book->string a-book)
  (string-append  (book-title a-book)
                  ", "
                  (book-author a-book)
                  " ("
                  (book-num-pages a-book)
                  "pp)," 
                  (if (book-is-copyrighted? a-book) "Copyrighted" "")
    )
  )

(check-expect (book->string (make-book "The Cat in the Hat" "Suess" 37 #true)
              ) "The Cat in the Hat, by Suess (37pp), Copyrighted")