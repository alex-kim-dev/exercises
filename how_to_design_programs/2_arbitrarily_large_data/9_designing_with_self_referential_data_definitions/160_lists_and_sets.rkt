;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 160_lists_and_sets) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Two data representations for sets of numbers:

; A Son.L is one of:
; – empty
; – (cons Number Son.L)
; Son is used when it applies to Son.L and Son.R

; A Son.R is one of:
; – empty
; – (cons Number Son.R)
; Constraint: If s is a Son.R, no number occurs twice in s

; 160 Design the functions set+.L and set+.R

; Son.L Number -> Son.L
; adds a number to the set
(check-expect (set+.L (list 1 2 3) 3) (list 3 1 2 3))
(check-expect (set+.L (list 1 2 3) 4) (list 4 1 2 3))
(define (set+.L set n)
  (cons n set))

; Son.R Number -> Son.R
; adds a number to the set
(check-expect (set+.R (list 1 2 3) 3) (list 1 2 3))
(check-expect (set+.R (list 1 2 3) 4) (list 4 1 2 3))
(define (set+.R set n)
  (if (member? n set) set (cons n set)))
