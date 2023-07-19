;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 137-155) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; 137 non-code
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

; 148-149 non-code

; 150 Design the function add-to-pi without using the primitive + operation,
; then generalize the function to just add

; Natural -> Number
; adds a natural number to pi
(check-within (add-to-pi 0) pi 0.001)
(check-within (add-to-pi 1) (+ 1 pi) 0.001)
(check-within (add-to-pi 3) (+ 3 pi) 0.001)
(define (add-to-pi n)
  (cond
    [(zero? n) pi]
    [else (add1 (add-to-pi (sub1 n)))]))

; Natural Number -> Number
; adds a natural number to a given x
(check-within (add 0 1.23) 1.23 0.001)
(check-within (add 1 1.23) (+ 1 1.23) 0.001)
(check-within (add 3 1.23) (+ 3 1.23) 0.001)
(define (add n x)
  (cond
    [(zero? n) x]
    [else (add1 (add (sub1 n) x))]))

; 151 Design the function multiply. It consumes a natural number n and multiplies it with a number x
; without using *.

; Narutal Number -> Number
; multiplies n by x
(check-expect (multiply 0 1.23) 0)
(check-expect (multiply 1 1.23) 1.23)
(check-within (multiply 2 1.23) 2.46 0.001)
(check-within (multiply 3 1.23) 3.69 0.001)
(define (multiply n x)
  (cond
    [(zero? n) 0]
    [(= n 1) x]
    [else (+ x (multiply (sub1 n) x))]))

; 152 Design two functions: col and row

(define CELL (rectangle 5 10 "solid" "gray"))

; Natural Image -> Image
; produces a column of n copies of img
(check-expect (col 0 CELL) (rectangle 5 0 "solid" "transparent"))
(check-expect (col 1 CELL) CELL)
(check-expect (col 2 CELL) (above CELL CELL))
(check-expect (col 3 CELL) (above CELL CELL CELL))
(define (col n img)
  (cond
    [(zero? n) (rectangle (image-width img) 0 "solid" "transparent")]
    [(= n 1) img]
    [else (above img (col (sub1 n) img))]))

; Natural Image -> Image
; produces a row of n copies of img
(check-expect (row 0 CELL) (rectangle 0 10 "solid" "transparent"))
(check-expect (row 1 CELL) CELL)
(check-expect (row 2 CELL) (beside CELL CELL))
(check-expect (row 3 CELL) (beside CELL CELL CELL))
(define (row n img)
  (cond
    [(zero? n) (rectangle 0 (image-height img) "solid" "transparent")]
    [(= n 1) img]
    [else (beside img (row (sub1 n) img))]))

; 153 visualize the result of a 1968-style European student riot

(define SQ-SIZE 10)
(define SQUARE (rectangle SQ-SIZE SQ-SIZE "outline" "black"))
(define DOT (circle 3 "solid" "red"))
(define COLUMNS 10)
(define ROWS 20)

(define HALL
  (place-image (col ROWS (row COLUMNS SQUARE))
               (/ (* SQ-SIZE COLUMNS) 2)
               (/ (* SQ-SIZE ROWS) 2)
               (empty-scene (* SQ-SIZE COLUMNS) (* SQ-SIZE ROWS))))

; List-of-posn -> Image
; paints the lecture hall with baloons
(define (add-balloons list)
  (cond
    [(empty? (rest list)) (underlay/xy HALL (posn-x (first list)) (posn-y (first list)) DOT)]
    [else (underlay/xy (add-balloons (rest list)) (posn-x (first list)) (posn-y (first list)) DOT)]))

(add-balloons (list (make-posn 50 10)
                    (make-posn 16 2)
                    (make-posn 94 180)
                    (make-posn 56 134)
                    (make-posn 11 100)
                    (make-posn 65 15)))

; 154 Design the function colors. It consumes a Russian doll and produces a string of all colors,
; separated by a comma and a space.

(define-struct layer [color doll])

; RussianDoll is one of:
; – String
; – (make-layer String RussianDoll)

; RussianDoll -> Number
; how many dolls are a part of a doll
(check-expect (depth "red") 1)
(check-expect (depth (make-layer "yellow" (make-layer "green" "red"))) 3)
(define (depth doll)
  (cond
    [(string? doll) 1]
    [else (+ (depth (layer-doll doll)) 1)]))

; RussianDoll -> String
; produces a string of all colors within a doll
(check-expect (colors "red") "red")
(check-expect (colors (make-layer "green" "red")) "green, red")
(check-expect (colors (make-layer "yellow" (make-layer "green" "red"))) "yellow, green, red")
(define (colors doll)
  (cond
    [(string? doll) doll]
    [else (string-append (layer-color doll) ", " (colors (layer-doll doll)))]))

; 155 Design the function inner, which consumes an doll and produces the (color of the) innermost one.

; RussianDoll -> String
; returns the color of the innermost doll
(check-expect (inner "red") "red")
(check-expect (inner (make-layer "green" "red")) "red")
(check-expect (inner (make-layer "yellow" (make-layer "green" "red"))) "red")
(define (inner doll)
  (cond
    [(string? doll) doll]
    [else (inner (layer-doll doll))]))
