;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hw04b) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Hw04b http://www.radford.edu/~itec380/2015fall-ibarland/Homeworks/hw04b.html
; Jordan Taylor
; ITEC 380
; A team is:
;  - a name (non-empty string), AND
;  - an offense rating (real number), AND
;  - a defense rating (real number)
; (make-team [non-empty string] [real] [real]
(define-struct team(name offense defense))
(define t1 (make-team "Redskins" 100 110))
(define t2 (make-team "Cardinals" 200 200))
(define t3 (make-team "Highlanders" 150 100))

; template function for a team

(define (func-for-team a-team)
  (... (team-name a-team)
   ... (team-offense a-team)
   ... (team-defense a-team)
   )
  )
(define (team>? a-team b-team)
  (cond [(> (team-offense a-team) (team-defense b-team))
           (string-append "The winner is " (team-name a-team))]
        [(< (team-offense a-team) (team-defense b-team))
           (string-append "The winner is " (team-name b-team))]
        [(= (team-offense a-team) (team-defense b-team))
           (string-append "The winner is neither")]
        
   )
  )
(check-expect (team>? t1 t2) "The winner is Cardinals")
(check-expect (team>? t2 t2) "The winner is neither")
(check-expect (team>? t3 t2) "The winner is Cardinals")
(check-expect (team>? t1 t3) "The winner is neither")
(check-expect (team>? t3 t1) "The winner is Highlanders")
(check-expect (team>? t1 t1) "The winner is Redskins")
(check-expect (team-name t1) "Redskins")

; Three objects in the game space invaders
;  1. Spaceship (or) Player Controlled Craft
;  2. Aliens 
;  3. Red UFO 

; A spaceship is:
;  - num-lives : total amount of lives before the game ends
;  - score : total score of the player currently playing
; (make-spaceship [non-negative real] [non-negative real]

(define-struct spaceship(num-lives, score))
(define s1 (make-spaceship 3 3000))
(define s2 (make-spaceship 1 400))
(define s3 (make-spaceship 2 1200))



