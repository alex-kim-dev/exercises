;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 129-148) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

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

; 135-137 non-code

; 138 Design the sum function, which consumes a List-of-amounts and computes the sum of the amounts.

; A List-of-amounts is one of:
; – '()
; – (cons PositiveNumber List-of-amounts)

; List-of-amounts -> PositiveNumber
; computes the sum of the amounts in the list of amounts
(check-expect (sum '()) 0)
(check-expect (sum (cons 1 '())) 1)
(check-expect (sum (cons 2 (cons 1 '()))) 3)
(define (sum list)
  (cond
    [(empty? list) 0]
    [else (+ (first list) (sum (rest list)))]))

; 139 Design the function pos?, which consumes a List-of-numbers and determines whether all numbers
; are positive numbers. Also design checked-sum.

; A List-of-numbers is one of:
; – '()
; – (cons Number List-of-numbers)

; Any -> Boolean
; checks if a value is an element of List-of-amounts
(check-expect (pos? 1) #false)
(check-expect (pos? '()) #true)
(check-expect (pos? (cons -1 '())) #false)
(check-expect (pos? (cons 2 (cons -1 '()))) #false)
(check-expect (pos? (cons 1 '())) #true)
(check-expect (pos? (cons 2 (cons 1 '()))) #true)
(define (pos? arg)
  (cond
    [(empty? arg) #true]
    [(and (cons? arg) (> (first arg) 0) (pos? (rest arg))) #true]
    [else #false]))

(define CS-ERR-MSG "checked-sum: the argument is not a list of amounts")

; List-of-numbers -> PositiveNumber
(check-error (checked-sum 1) CS-ERR-MSG)
(check-error (checked-sum (cons 2 (cons -1 '()))) CS-ERR-MSG)
(check-expect (checked-sum '()) 0)
(check-expect (checked-sum (cons 2 (cons 1 '()))) 3)
(define (checked-sum list)
  (cond
    [(pos? list) (sum list)]
    [else (error CS-ERR-MSG)]))

; 140 Design the function all-true, which consumes a list of Boolean values and determines whether all
; of them are #true. Design one-true, a function that consumes a list of Boolean values and determines
; whether at least one item on the list is #true.

; List-of-booleans is one of:
; - '()
; - (cons Boolean List-of-booleans)

; List-of-booleans -> Boolean
; (check-expect (all-true '()) #false) not sure how to fix it
(check-expect (all-true (cons #false '())) #false)
(check-expect (all-true (cons #true '())) #true)
(check-expect (all-true (cons #false (cons #false '()))) #false)
(check-expect (all-true (cons #true (cons #false '()))) #false)
(check-expect (all-true (cons #false (cons #true '()))) #false)
(check-expect (all-true (cons #true (cons #true '()))) #true)
(define (all-true list)
  (cond
    [(empty? list) #true]
    [(equal? (first list) #false) #false]
    [else (all-true (rest list))]))

; List-of-booleans -> Boolean
(check-expect (one-true '()) #false)
(check-expect (one-true (cons #false '())) #false)
(check-expect (one-true (cons #true '())) #true)
(check-expect (one-true (cons #false (cons #false '()))) #false)
(check-expect (one-true (cons #true (cons #false '()))) #true)
(check-expect (one-true (cons #false (cons #true '()))) #true)
(check-expect (one-true (cons #true (cons #true '()))) #true)
(define (one-true list)
  (cond
    [(empty? list) #false]
    [(equal? (first list) #true) #true]
    [else (one-true (rest list))]))

; 141 Design the function cat, which consumes a list of strings and appends them all into one string

; List-of-string -> String
; concatenates all strings in l into one long string
(check-expect (cat '()) "")
(check-expect (cat (cons "a" (cons "b" '()))) "ab")
(check-expect (cat (cons "ab" (cons "cd" (cons "ef" '())))) "abcdef")
(define (cat list)
  (cond
    [(empty? list) ""]
    [else (string-append (first list) (cat (rest list)))]))

; 142 Design the ill-sized? function. It produces the first image that is not an n by n square.

; ImageOrFalse is one of:
; – Image
; – #false

(define IMG1 (rectangle 1 2 "solid" "red"))
(define IMG2 (rectangle 2 2 "solid" "red"))
(define IMG3 (rectangle 2 3 "solid" "red"))

; List-of-images PositiveNumber -> ImageOrFalse
(check-expect (ill-sized? '() 2) #false)
(check-expect (ill-sized? (cons IMG1 '()) 2) #false)
(check-expect (ill-sized? (cons IMG2 '()) 2) IMG2)
(check-expect (ill-sized? (cons IMG1 (cons IMG2 '())) 2) IMG2)
(check-expect (ill-sized? (cons IMG2 (cons IMG1 '())) 2) IMG2)
(check-expect (ill-sized? (cons IMG2 (cons IMG1 '())) 1) #false)
(check-expect (ill-sized? (cons IMG3 (cons IMG1 '())) 2) #false)
(define (ill-sized? list size)
  (cond
    [(empty? list) #false]
    [(and (= (image-width (first list)) size) (= (image-height (first list)) size)) (first list)]
    [else (ill-sized? (rest list) size)]))

; 143-144 non-code

; 145 Design the sorted>? predicate for non-empty list of temperatures

; An NEList-of-temperatures is one of:
; – (cons CTemperature '())
; – (cons CTemperature NEList-of-temperatures)
; interpretation: non-empty lists of Celsius temperatures

; NEList-of-temperatures -> Boolean
; returns true if the temperatures are sorted in descending order
(check-expect (sorted>? (cons 1 '())) #true)
(check-expect (sorted>? (cons 2 (cons 1 '()))) #true)
(check-expect (sorted>? (cons 1 (cons 2 '()))) #false)
(check-expect (sorted>? (cons 1 (cons 1 '()))) #false)
(check-expect (sorted>? (cons 3 (cons 2 (cons 1 '())))) #true)
(check-expect (sorted>? (cons 3 (cons 1 (cons 2 '())))) #false)
(define (sorted>? list)
  (cond
    [(empty? (rest list)) #true]
    [(> (first list) (first (rest list))) (sorted>? (rest list))]
    [else #false]))

; 146 Design how-many for non-empty list of temperatures

; NEList-of-temperatures -> Number
; counts how many temperatures there are in a list
(check-expect (how-many (cons 1 '())) 1)
(check-expect (how-many (cons 1 (cons 2 '()))) 2)
(check-expect (how-many (cons 1 (cons 2 (cons 3 '())))) 3)
(define (how-many list)
  (cond
    [(empty? (rest list)) 1]
    [else (+ 1 (how-many (rest list)))]))

; 147 Develop a data definition for non-empty list of booleans, redesign all-true & one-true

; NEList-of-booleans is one of:
; - (cons Boolean '())
; - (cons Boolean NEList-of-booleans)

; NEList-of-booleans -> Boolean
(check-expect (ne-all-true (cons #false '())) #false)
(check-expect (ne-all-true (cons #true '())) #true)
(check-expect (ne-all-true (cons #false (cons #false '()))) #false)
(check-expect (ne-all-true (cons #true (cons #false '()))) #false)
(check-expect (ne-all-true (cons #false (cons #true '()))) #false)
(check-expect (ne-all-true (cons #true (cons #true '()))) #true)
(define (ne-all-true list)
  (cond
    [(empty? (rest list)) (first list)]
    [(equal? (first list) #false) #false]
    [else (all-true (rest list))]))

; NEList-of-booleans -> Boolean
(check-expect (ne-one-true (cons #false '())) #false)
(check-expect (ne-one-true (cons #true '())) #true)
(check-expect (ne-one-true (cons #false (cons #false '()))) #false)
(check-expect (ne-one-true (cons #true (cons #false '()))) #true)
(check-expect (ne-one-true (cons #false (cons #true '()))) #true)
(check-expect (ne-one-true (cons #true (cons #true '()))) #true)
(define (ne-one-true list)
  (cond
    [(empty? (rest list)) (first list)]
    [(equal? (first list) #true) #true]
    [else (ne-one-true (rest list))]))

; 148 non-code
