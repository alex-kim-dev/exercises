;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 129-136) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; 129-133 basic lists stuff

; 134 Develop the contains? function, which determines whether some given string
; occurs on a given list of strings.

; List String -> Boolean
; determines whether a list contains some string

(check-expect (contains? '() "a") #false)
(check-expect (contains? (cons "a" '()) "b") #false)
(check-expect (contains? (cons "a" '()) "a") #true)
(check-expect (contains? (cons "a" (cons "b" (cons "c" '()))) "c") #true)
(check-expect (contains? (cons "a" (cons "b" (cons "c" '()))) "d") #false)

(define (contains? list str)
  (cond
    [(empty? list) #false]
    [else (or (string=? (first list) str) (contains? (rest list) str))]))

; 135-136 non-code
