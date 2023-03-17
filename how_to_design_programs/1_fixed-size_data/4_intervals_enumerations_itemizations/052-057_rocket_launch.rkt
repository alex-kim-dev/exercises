;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 052-057_rocket_launch) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; 52-57 Design a program that launches a rocket.
; The program first displays the rocket sitting at the bottom of the canvas.
; When the space bar is pressed, the simulation starts a countdown for three ticks, before it displays the scenery of a rising rocket.
; The rocket should move upward at a rate of three pixels per clock tick.
; - 2 interpretations of height: distance between the ground/top and the rocket

; An LRCD (for launching rocket countdown) is one of:
; – "resting"
; – a Number between -3 and -1
; – a NonnegativeNumber
; interpretation: a grounded rocket, in countdown mode,
; a number denotes the number of pixels between the
; top of the canvas and the rocket (its height)

(define HEIGHT 150) ; distances in pixels
(define WIDTH 100)
(define YDELTA 3)
(define BACKG (empty-scene WIDTH HEIGHT))
(define ROCKET (rectangle 5 30 "solid" "red"))
(define CENTER (/ (image-height ROCKET) 2))

; Number -> Image
; draw the rocket at specified height
(define (place-rocket height)
  (place-image ROCKET 50 (- height CENTER) BACKG))

; LRCD -> Image
; renders the state as a resting or flying rocket
(check-expect (show "resting") (place-rocket HEIGHT))
(check-expect (show -2) (place-image (text "-2" 20 "red") 50 (* 3/4 WIDTH) (place-rocket HEIGHT)))
(check-expect (show 53) (place-rocket 53))
(check-expect (show HEIGHT) (place-rocket HEIGHT))
(check-expect (show 0) (place-rocket 0))
(define (show x)
  (cond
    [(string? x) (place-rocket HEIGHT)]
    [(<= -3 x -1)
     (place-image (text (number->string x) 20 "red") 50 (* 3/4 WIDTH) (place-rocket HEIGHT))]
    [(>= x 0) (place-rocket x)]))

; LRCD KeyEvent -> LRCD
; starts the countdown when space bar is pressed,
; if the rocket is still resting
(check-expect (launch "resting" " ") -3)
(check-expect (launch "resting" "a") "resting")
(check-expect (launch -3 " ") -3)
(check-expect (launch -1 " ") -1)
(check-expect (launch 33 " ") 33)
(check-expect (launch 33 "a") 33)
(define (launch x key)
  (cond
    [(string? x) (if (string=? " " key) -3 x)]
    [(<= -3 x -1) x]
    [(>= x 0) x]))

; LRCD -> LRCD
; raises the rocket by YDELTA, if it is moving already
(check-expect (fly "resting") "resting")
(check-expect (fly -3) -2)
(check-expect (fly -2) -1)
(check-expect (fly -1) HEIGHT)
(check-expect (fly 10) (- 10 YDELTA))
(check-expect (fly 22) (- 22 YDELTA))
; LRCD -> LRCD
; raises the rocket by YDELTA if it is moving already

(check-expect (fly "resting") "resting")
(check-expect (fly -3) -2)
(check-expect (fly -2) -1)
(check-expect (fly -1) HEIGHT)
(check-expect (fly 10) (- 10 YDELTA))
(check-expect (fly 22) (- 22 YDELTA))
(define (fly x)
  (cond
    [(string? x) x]
    [(<= -3 x -1) (if (= x -1) HEIGHT (+ x 1))]
    [(>= x 0) (- x YDELTA)]))

; LRCD -> Boolean
; Stops program when the rocket reaches the top of the scene
(define (end? x)
  (if (number? x) (= x 0) #false))

; LRCD -> LRCD
(define (launch-rocket s)
  (big-bang s [on-tick fly 1/2] [to-draw show] [on-key launch] [stop-when end?]))

(launch-rocket "resting")