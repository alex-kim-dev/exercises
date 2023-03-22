;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 110-115) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

; 110 Write a type checked area-of-disk for positive numbers

; Any -> Number
; computes the area of a disk with radius value,
; if value is a positive number
(define (area-of-disk value)
  (cond
    [(and (number? value) (>= value 0)) (* pi (sqr value))]
    [else (error "area-of-disk: a positive number expected")]))

; (area-of-disk -1)
; (area-of-disk "-1")
(area-of-disk 0)
(area-of-disk 2)

; 111 Develop the function checked-make-vec

(define-struct vec [x y])
; A vec is (make-vec PositiveNumber PositiveNumber)
; represents a velocity vector

; Any -> Boolean
; predicate, determines is the passed argument is a positive number
(define (positive-number? arg)
  (and (number? arg) (>= arg 0)))

; Any -> Vec
; creates a new velocity vector, checks the type of arguments
(define (checked-make-vec x y)
  (if (and (positive-number? x) (positive-number? y))
      (make-vec x y)
      (error "make-vec: expected 2 positive numbers")))

; (checked-make-vec 1 "1")
; (checked-make-vec 1 -1)
(checked-make-vec 3 5)

; 112 Reformulate the MissileOrNot? predicate now using an or expression

; Any -> Boolean
; is a an element of the MissileOrNot collection
(check-expect (missile-or-not? #false) #true)
(check-expect (missile-or-not? (make-posn 9 2)) #true)
(check-expect (missile-or-not? "yellow") #false)
(check-expect (missile-or-not? #true) #false)
(check-expect (missile-or-not? 10) #false)
(check-expect (missile-or-not? empty-image) #false)
(define (missile-or-not? arg)
  (cond
    [(or (false? arg) (posn? arg)) #true]
    [else #false]))

; 113 Design predicates for the following data definitions from the preceding section:
; space invaders state, Coordinate (exercise 105), and VAnimal

; UFO: Posn
; Missile: Posn
; Tank: (make-tank Number Number)
(define-struct tank [x vel])
(define-struct aim [ufo tank])
(define-struct fired [ufo tank missile])
; SiState is one of:
; - (make-aim UFO Tank)
; - (make-fired UFO Tank Missile)

; Any -> Boolean
(check-expect (sistate? (make-aim (make-posn 0 0) (make-tank 0 0))) #true)
(check-expect (sistate? (make-fired (make-posn 0 0) (make-tank 0 0) (make-posn 0 0))) #true)
(check-expect (sistate? 0) #false)
(check-expect (sistate? (make-posn 0 0)) #false)
(define (sistate? arg)
  (cond
    [(or (aim? arg) (fired? arg)) #true]
    [else #false]))

; A Coordinate is one of:
; – a NegativeNumber
; – a PositiveNumber
; – a Posn

; Any -> Boolean
(check-expect (coordinate? -1) #true)
(check-expect (coordinate? 1) #true)
(check-expect (coordinate? 0) #true)
(check-expect (coordinate? (make-posn 0 0)) #true)
(check-expect (coordinate? "1") #false)
(define (coordinate? arg)
  (cond
    [(number? arg) #true]
    [(posn? arg) #true]
    [else #false]))

; VCat: (make-vcat Number Happiness Direction)
(define-struct vcat [x hap dir])
; VCham: (make-vcham Number Happiness Color)
(define-struct vcham [x hap color])
; VAnimal is one of: VCat, VCham

; Any -> Boolean
(check-expect (vanimal? (make-vcat 0 0 1)) #true)
(check-expect (vanimal? (make-vcham 0 0 "red")) #true)
(check-expect (vanimal? (make-posn 0 0)) #false)
(check-expect (vanimal? 1) #false)
(define (vanimal? arg)
  (cond
    [(or (vcat? arg) (vcham? arg)) #true]
    [else #false]))

; 114 Update the space invader game, the virual pet game, the editor program
; so they use predicates to check state

; 115 evise light=? so that the error message specifies
; which of the two arguments isn’t an element of TrafficLight.

; Any -> Boolean
; is the given value an element of TrafficLight
(define (light? arg)
  (cond
    [(string? arg) (or (string=? "red" arg) (string=? "green" arg) (string=? "yellow" arg))]
    [else #false]))

(define MESSAGE "traffic light expected, given some other value")

; Any Any -> Boolean
; are the two values elements of TrafficLight and, if so, are they equal
(check-expect (light=? "red" "red") #true)
(check-expect (light=? "red" "green") #false)
(check-expect (light=? "green" "green") #true)
(check-expect (light=? "yellow" "yellow") #true)
(define (light=? arg1 arg2)
  (if (and (light? arg1) (light? arg2))
      (string=? arg1 arg2)
      (cond
        [(boolean=? (and (light? arg1) (light? arg2)) #false)
         (error "The both args should be of type TrafficLight")]
        [(boolean=? (light? arg1) #false) (error "The 1st arg should be of type TrafficLight")]
        [(boolean=? (light? arg2) #false) (error "The 2nd arg should be of type TrafficLight")]
        [else (error MESSAGE)])))
