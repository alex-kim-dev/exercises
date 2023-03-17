;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 063-082) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; 63 non-code

; 64 Design the function manhattan-distance, which measures the Manhattan distance of the given posn to the origin
; Point -> Distance
; where Point is a pair of Numbers, Distance is a Number
(check-expect (manhattan-distance (make-posn 0 0)) 0)
(check-expect (manhattan-distance (make-posn 3 0)) 3)
(check-expect (manhattan-distance (make-posn 0 5)) 5)
(check-expect (manhattan-distance (make-posn 2 4)) 6)
(define (manhattan-distance point)
  (+ (posn-x point) (posn-y point)))

; 65-72 non-code

; 73 Design the function posn-up-x, which consumes a Posn p and a Number n. It produces a Posn like p with n in the x field.
; 74 Make the placing a dot program run

(define SCENE (empty-scene 100 100))
(define DOT (circle 3 "solid" "red"))

; A Posn represents the state of the world.

; Posn -> Image
; adds a red spot to MTS at p
(check-expect (renderDot (make-posn 10 20)) (place-image DOT 10 20 SCENE))
(check-expect (renderDot (make-posn 88 73)) (place-image DOT 88 73 SCENE))
(define (renderDot point)
  (place-image DOT (posn-x point) (posn-y point) SCENE))

; Posn -> Posn
; increases the x-coordinate of p by 3
(check-expect (x+ (make-posn 10 0)) (make-posn 13 0))
(define (x+ point)
  (make-posn (+ (posn-x point) 3) (posn-y point)))

; Posn -> Posn
; swaps the x coordinate in a Posn with a given number
(check-expect (posn-up-x (make-posn 0 0) 1) (make-posn 1 0))
(check-expect (posn-up-x (make-posn 6 7) 3) (make-posn 3 7))
(define (posn-up-x point n)
  (make-posn n (posn-y point)))

; Posn Number Number MouseEvt -> Posn
; for mouse clicks, (make-posn x y); otherwise p
(check-expect (reset-dot (make-posn 10 20) 29 31 "button-down") (make-posn 29 31))
(check-expect (reset-dot (make-posn 10 20) 29 31 "button-up") (make-posn 10 20))
(define (reset-dot point x y mouseEvent)
  (cond
    [(mouse=? mouseEvent "button-down") (make-posn x y)]
    [else point]))

; Posn -> Posn
(define (canvas-dot initialPoint)
  (big-bang initialPoint [on-tick x+] [on-mouse reset-dot] [to-draw renderDot]))

(canvas-dot (make-posn 50 50))

; a game program that keeps track of an object that moves across the canvas at changing speed:
; 75 test handlers

; Vel: (make-vel Number Number)
; interpretation: velocity of a 2d object
(define-struct vel [deltax deltay])

(define-struct ufo [loc vel])
; A UFO is a structure:
;   (make-ufo Posn Vel)
; interpretation (make-ufo p v) is at location
; p moving at velocity v

(define v1 (make-vel 8 -3))
(define v2 (make-vel -5 -3))
(define p1 (make-posn 22 80))
(define p2 (make-posn 30 77))
(define u1 (make-ufo p1 v1))
(define u2 (make-ufo p1 v2))
(define u3 (make-ufo p2 v1))
(define u4 (make-ufo p2 v2))

; Posn Vel -> Posn
; adds velocity to location
(check-expect (posn+ p1 v1) p2)
(check-expect (posn+ p1 v2) (make-posn 17 77))
(define (posn+ point velocity)
  (make-posn (+ (posn-x point) (vel-deltax velocity)) (+ (posn-y point) (vel-deltay velocity))))

; UFO -> UFO
; determines where u moves in one clock tick;
; leaves the velocity as is
(check-expect (ufo-move-1 u1) u3)
(check-expect (ufo-move-1 u2) (make-ufo (make-posn 17 77) v2))
(define (ufo-move-1 ufo)
  (make-ufo (posn+ (ufo-loc ufo) (ufo-vel ufo)) (ufo-vel ufo)))

; 76 non-code

; 77 Provide a structure type definition and a data definition for representing points in time since midnight. A point in time consists of three numbers: hours, minutes, and seconds.

; Time: (make-time Number Number Number)
; interpretation: time in hours, mins, seconds since midnight
(define-struct time [h m s])

; 78 Provide a structure type and a data definition for representing three-letter words. A word consists of lowercase letters, represented with the 1Strings "a" through "z" plus #false.

; A Letter is one of:
; - a-z
; - #false
; interpretation: one of the characters of a word in the hangman game

; WordOf3: (make-wordOf3 Letter Letter Letter)
; interpretation: a three-letter word in a the hangman game
(define-struct wordOf3 [a b c])

; 79 Create examples for the following data definitions

; A Color is one of:
; — "white"
; — "yellow"
; — "orange"
; — "green"
; — "red"
; — "blue"
; — "black"
; Examples: "white", "black", "green"

; H is a Number between 0 and 100.
; interpretation represents a happiness value
; Examples: 0, 45, 70 100

(define-struct person [fstname lstname male?])
; A Person is a structure:
;   (make-person String String Boolean)
; Examples:
; (make-person "Alex" "Kim" #true)
; (make-person "" "" #false)

(define-struct dog [owner name age happiness])
; A Dog is a structure:
;   (make-dog Person String PositiveInteger H)
; Examples:
; (make-dog "Alex" "Doge" 10 50)
; (make-dog "Mia" "Pupp" 1 70)

; A Weapon is one of:
; — #false
; — Posn
; interpretation #false means the missile hasn't
; been fired yet; a Posn means it is in flight
; Examples: #false, (make-posn 0 0), (make-posn 13 21)

; 80 Create templates

(define-struct movie [title director year])
; (define (movieFn movie)
;   (... (movie-title movie)
;    ... (movie-director movie)
;    ... (movie-year movie) ...))

(define-struct pet [name number])
; (define (petFn pet)
;   (... (pet-name pet) ... (pet-number) ... ))

(define-struct CD [artist title price])
; (define (CDFn CD)
;   (... (CD-artist CD)
;    ... (CD-title CD)
;    ... (CD-price CD) ...))

(define-struct sweater [material size color])
; (define (sweaterFn sweater)
;   (... (sweater-material sweater)
;    ... (sweater-size sweater)
;    ... (sweater-color sweater) ...))

; 81 Design the function time->seconds

; Time -> Number
; produces the number of seconds since midnight
(check-expect (time->seconds (make-time 0 0 0)) 0)
(check-expect (time->seconds (make-time 0 0 10)) 10)
(check-expect (time->seconds (make-time 0 2 5)) 125)
(check-expect (time->seconds (make-time 2 3 0)) 7380)
(define (time->seconds time)
  (+ (* (time-h time) 3600) (* (time-m time) 60) (time-s time)))

; 82 Design the function compare-word.

(define ABC (make-wordOf3 "a" "b" "c"))
(define ABD (make-wordOf3 "a" "b" "d"))
(define AB_ (make-wordOf3 "a" "b" #false))
(define _BC (make-wordOf3 #false "b" "c"))
(define _B_ (make-wordOf3 #false "b" #false))

; Letter Letter -> Boolean
; checks if 2 letters are equal
(check-expect (letter=? "a" "a") #true)
(check-expect (letter=? "a" "b") #false)
(check-expect (letter=? "a" "") #false)
(check-expect (letter=? "" "") #true)
(check-expect (letter=? "a" #false) #false)
(check-expect (letter=? #false #false) #false)
(define (letter=? l1 l2)
  (cond
    [(boolean? l1) #false]
    [(boolean? l2) #false]
    [else (string=? l1 l2)]))

; WordOf3 WordOf3 -> WordOf3
; produces a word that indicates where the given ones agree and disagree
(check-expect (compare-word ABC ABC) ABC)
(check-expect (compare-word ABC ABD) AB_)
(check-expect (compare-word ABC _BC) _BC)
(check-expect (compare-word AB_ _BC) _B_)
(define (compare-word word1 word2)
  (make-wordOf3 (if (letter=? (wordOf3-a word1) (wordOf3-a word2)) (wordOf3-a word1) #false)
                (if (letter=? (wordOf3-b word1) (wordOf3-b word2)) (wordOf3-b word1) #false)
                (if (letter=? (wordOf3-c word1) (wordOf3-c word2)) (wordOf3-c word1) #false)))