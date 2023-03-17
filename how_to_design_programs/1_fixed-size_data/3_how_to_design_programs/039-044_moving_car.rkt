;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 039-044_moving_car) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

; MOVING CAR - a set of exercises
; 39 Make an image of a car so that WHEEL-RADIUS remains the single point of control
; 40 Write tests for the tock fn using check-expect
; 41 Make the program run, add a tree, add end?
; 42 Make modifications so x relates to right-most edge of the car
; 43 Change to time-based state, try sine wave movement
; 44 Write tests for hyper (mouse handler)

; scene
(define SCENE-HEIGHT 30)
(define SCENE-WIDTH 300)
(define SCENE (empty-scene SCENE-WIDTH SCENE-HEIGHT))
(define PX-PER-TICK 3)

; car
(define WHEEL-RADIUS 5)
(define WHEEL-DISTANCE (* WHEEL-RADIUS 2))
(define STEP WHEEL-DISTANCE)
(define CAR
  (underlay/xy (polygon (list (make-posn STEP 0)
                              (make-posn (* 3 STEP) 0)
                              (make-posn (* 4 STEP) STEP)
                              (make-posn (* 5 STEP) STEP)
                              (make-posn (* 5 STEP) (* 2 STEP))
                              (make-posn 0 (* 2 STEP))
                              (make-posn 0 STEP))
                        "solid"
                        "red")
               WHEEL-RADIUS
               (* 3 WHEEL-RADIUS)
               (underlay/xy (circle WHEEL-RADIUS "solid" "black")
                            (* 3 STEP)
                            0
                            (circle WHEEL-RADIUS "solid" "black"))))

; An AnimationState is a Number.
; interpretation: the number of clock ticks
; since the animation started

; TODO
; [x] render
; [x] tick-handler
; [ ] keystroke-handler
; [x] mouse-event-handler
; [x] end?

; AnimationState -> Image
; places the image of the car PX-PER-TICK pixels from the left
; margin of the SCENE + car width every tick
(define (render n)
  (place-image/align CAR (* PX-PER-TICK n) SCENE-HEIGHT "right" "bottom" SCENE))

; AnimationState -> AnimationState
; increments the clock
(check-expect (tock 20) 21)
(check-expect (tock 78) 79)
(define (tock n)
  (+ n 1))

; AnimationState Number Number String -> AnimationState
; places the car at x-mouse
; if the given me is "button-down"
(check-expect (hyper 21 10 20 "enter") 21)
(check-expect (hyper 42 10 20 "button-down") 3)
(check-expect (hyper 42 10 20 "move") 42)
(define (hyper x-position-of-car x-mouse y-mouse me)
  (cond
    [(string=? "button-down" me) (quotient x-mouse PX-PER-TICK)]
    [else x-position-of-car]))

; AnimationState -> Boolean
; stops the program after the car has disappeared on the right
(check-expect (end? 0) #false)
(check-expect (end? 10) #false)
(check-expect (end? (+ 1 (quotient SCENE-WIDTH PX-PER-TICK))) #true)
(define (end? n)
  (> (* PX-PER-TICK n) SCENE-WIDTH))

(define (main state)
  (big-bang state [on-tick tock] [on-mouse hyper] [to-draw render] [stop-when end?]))

(main 0)
