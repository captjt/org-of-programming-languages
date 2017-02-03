;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname turn) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Task : 'turn', for changing a stop-light
; Data definition
; A slc ("stoplight color") is one of:
;  'red'
;  'yellow'
;  'green'
;  'flashing-yellow'
;  'flashing-red'

; turn : slc -> slc
; Return what color a stop-light will be, after it's next turns

(define (turn current-color)
  (cond [(symbol=? current-color 'red)             'green]
        [(symbol=? current-color 'yellow)          'red]
        [(symbol=? current-color 'green)           'yellow]
        [(symbol=? current-color 'flashing-yellow) 'flashing-yellow]
        [(symbol=? current-color 'flashing-red)    'flashing-red]
        [else                    'whoa-nelly-something-is-wrong]
  )
)

(check-expect (turn 'yellow) 'red)
(check-expect (turn 'green) 'yellow)
(check-expect (turn 'red) 'green)
(check-expect (turn 'flashing-red) 'flashing-red)
(check-expect (turn 'flashing-yellow) 'flashing-yellow)



; Task write a penalty-for-running-a-light
; penalty-for-running-a-light : slc -> non-negative real
; Return a non-negative real value dollars for stop lights ran

(define (penalty-for-running-a-light current-color)
  (cond [(symbol=? current-color 'red)             50]
        [(symbol=? current-color 'yellow)          0]
        [(symbol=? current-color 'green)           0]
        [(symbol=? current-color 'flashing-yellow) 25]
        [(symbol=? current-color 'flashing-red)    99]
  )
)  

(check-expect (penalty-for-running-a-light 'red) 50)
(check-expect (penalty-for-running-a-light 'yellow) 0)
(check-expect (penalty-for-running-a-light 'green) 0)
(check-expect (penalty-for-running-a-light 'flashing-yellow) 25)
(check-expect (penalty-for-running-a-light 'flashing-red) 99)


; stop-light-color-template functions
(define (template-for-slc-function a-slc)
  (cond [(symbol=? a-slc 'red)             ...]
        [(symbol=? a-slc 'yellow)          ...]
        [(symbol=? a-slc 'green)           ...]
        [(symbol=? a-slc 'flashing-yellow) ...]
        [(symbol=? a-slc 'flashing-red)    ...]
  )
)

; templates save us time when we are using the data definitions
; we can reuse these for different functions for the same data definitions
