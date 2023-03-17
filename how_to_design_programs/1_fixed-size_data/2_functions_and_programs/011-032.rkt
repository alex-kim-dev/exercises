;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 011-032) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/batch-io)
(require 2htdp/universe)

; 11 Cartisian distance from a point to the origin
(define (distance x y)
  (sqrt (+ (sqr (- x 0)) (sqr (- y 0)))))
(distance 3 4)

; 12 Volume of a equilateral cube
(define (csurface side)
  (sqr side))
(define (cvolume side)
  (* (csurface side) side))
(cvolume 5)

; 13 Extract the first char of a non-empty string
(define (string-first str)
  (substring str 0 1))
(string-first "hello")

; 14 Extract the last char of a non-empty string
(define (string-last str)
  (substring str (- (string-length str) 1) (string-length str)))
(string-last "hello")

; 15 Boolean implication
(define (==> sunny friday)
  (or (equal? sunny #false) (equal? friday #true)))
(==> #false #true)
(==> #true #false)

; 16 Number of px
(define (image-area img)
  (* (image-width img) (image-height img)))
(image-area (rectangle 10 20 "solid" "blue"))

; 17 Whether an image is tall / wide / square
(define (image-classify img)
  (if (>= (image-height img) (image-width img))
      (if (= (image-height img) (image-width img)) "square" "tall")
      "wide"))
(image-classify (rectangle 10 20 "solid" "blue"))
(image-classify (rectangle 20 10 "solid" "blue"))
(image-classify (rectangle 20 20 "solid" "blue"))

; 18 Join strings with underscore
(define (string-join str1 str2)
  (string-append str1 "_" str2))
(string-join "hello" "world")

; 19 Insert underscore at ith position
(define (string-insert str i)
  (if (= (string-length str) 0)
      "_"
      (string-join (substring str 0 i) (substring str i (string-length str)))))
(string-insert "" 1)
(string-insert "hello" 3)

; 20 Delete a char at ith position
(define (string-delete str i)
  (if (= (string-length str) 0)
      ""
      (string-append (substring str 0 i) (substring str (+ i 1) (string-length str)))))
(string-delete "" 1)
(string-delete "hello" 4)

; 21-26 Practicing with stepper

; 27 Refactor - remove magic numbers
; 28 Calculate the profit for several ticket prices
; 29 Modify initial cost
; 30 Define priceSensitivity as a computed constant
(define initialAttendees 120)
(define initialTicketPrice 5.0)
(define attendeesChange 15)
(define priceChange 0.1)
(define priceSensitivity (/ attendeesChange priceChange))
(define fixedCost 0) ; 180 initial
(define variableCost 0.04)

(define (attendees ticket-price)
  (- initialAttendees (* (- ticket-price initialTicketPrice) priceSensitivity)))

(define (revenue ticket-price)
  (* ticket-price (attendees ticket-price)))

(define (cost ticket-price)
  (+ fixedCost (* variableCost (attendees ticket-price))))

(define (profit ticket-price)
  (- (revenue ticket-price) (cost ticket-price)))

(profit 2.8)
(profit 2.9) ; best
(profit 3)

; 31 Testing the function "letter" with some input files; stepper

; 32 non-code
