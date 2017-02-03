;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname tree) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; An ancestor tree is
;   - 'unknown, OR
;   - child
; struct - name, eye color, year of birth, parents - mother and father
; A child is ...
; (make-child [string][symbol][int][ancestor-tree][ancestor-tree])

(define-struct child (name eye-color year-of-birth mother father))

; examples of AncTrees
'unknown
(make-child "Abe" 1901 'brown 'unknown 'unknown)
(make-child "Homer" 1949 'brown 'unknown
            (make-child "Abe" 1901 'brown 'unknown 'unknown))

; Template
; Func-for-ancTree : ancTree ->

(define (func-for-ancTree tr)
  (cond  [(symbol? tr)   ...]
         [(child? tr)    (...(child-name tr)
                             ...(child-eye-color tr)
                             ...(child-year-of-birth tr)
                             ...(func-for-ancTree (child-mother tr))
                             ...(func-for-ancTree (child-father tr)))]))

(define (size tr)
  (cond  [(symbol? tr)   0]
         [(child? tr)    (+ (size (child-mother tr))
                            1
                            (size (child-father tr)))]))

(check-expect (size 'unknown) 0)
(check-expect (size (make-child "Abe" 1901 'brown 'unknown 'unknown)) 1)
(check-expect (size (make-child "Homer"
                                1955
                                'brown
                                'unknown
                                (make-child "Abe" 1901 'brown 'unknown 'unknown))) 2)

(define abe (make-child "Abe" 1920 'blue 'unknown 'unknown))
(define mona (make-child "Mona" 1929 'brown 'unknown 'unknown))
(define jackie (make-child "Jackie" 1929 'brown 'unknown 'unknown))
(define marge (make-child "Marge" 1956 'blue jackie 'unknown))
(define homer (make-child "Homer" 1955 'brown mona abe))
(define bart (make-child "Bart" 1979 'brown marge homer))

(check-expect (size bart) 6)
(check-expect (size homer) 3)
(check-expect (size marge) 2)