;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 045-047_virtual_pet) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

; 45 Design a “virtual cat” world program that continuously moves the cat from left to right.
; - consumes the starting position of the cat
; - move 3 px per clock tick
; - when disappears on the right, reappears on the left
; 46 Render 2 images of the cat depending on whether x is odd

; An AnimationState is a Number.
; interpretation: the number of clock ticks since the animation started

(define CAT1 (bitmap "../cat.png"))
(define CAT2 (bitmap "../cat2.png"))
(define PX-PER-TICK 3)
(define SCENE-HEIGHT (image-height CAT1))
(define SCENE-WIDTH 300)
(define SCENE (empty-scene SCENE-WIDTH SCENE-HEIGHT))

; AnimationState -> Number
; calculates the position based on clock time
(check-expect (timeToPos 0) 0)
(check-expect (timeToPos 5) 15)
(define (timeToPos time)
  (* time PX-PER-TICK))

; Number -> AnimationState
; calculates the clock time based on position
(check-expect (posToTime 0) 0)
(check-expect (posToTime 15) 5)
(check-expect (posToTime 14) 4)
(define (posToTime pos)
  (quotient pos PX-PER-TICK))

; AnimationState -> Image
; draws the cat on the scene at a certain position depending on the animation state
(define (renderCat time)
  (place-image/align (cond
                       [(even? time) CAT1]
                       [else CAT2])
                     (modulo (timeToPos time) (+ SCENE-WIDTH (image-width CAT1)))
                     SCENE-HEIGHT
                     "right"
                     "bottom"
                     SCENE))

; AnimationState -> AnimationState
; increments the clock time by 1
(check-expect (tockCat 0) 1)
(check-expect (tockCat 5) 6)
(define (tockCat time)
  (+ time 1))

; Number -> AnimationState
; Takes in the starting position of a cat and animates the cat movement on the scene from left to right continuously
(define (cat-prog startPos)
  (big-bang (posToTime startPos) [on-tick tockCat] [to-draw renderCat]))

(cat-prog 50)

; 47 Design a world program that maintains and displays a “happiness gauge”.
; - consumes the maximum level H of happiness
; - the gauge display starts with the maximum score, and with each clock tick, happiness decreases by 0.1; it never falls below 0
; - on arrow down - decrease happiness by 1/5; on arrow up - increase by 1/3

; a HappinessState is a Number.
; interpretation: the number of clock ticks since the animation started

(define HAPPINESS-DECREMENT 0.1)
(define HAPPINESS-DECREASE 1/5)
(define HAPPINESS-INCREASE 1/3)
(define MIN-HAPPINESS 0)

(define GAUGE-HEIGHT 200)
(define GAUGE-WIDTH 50)
(define SCENE2 (empty-scene GAUGE-WIDTH GAUGE-HEIGHT))

(define maxH 10) ; How to use it without variables/passing a complex data structure?

; HappinessState -> Image
; draws the happiness gauge on the scene
(define (renderGauge h)
  (place-image/align (rectangle GAUGE-WIDTH (* (/ h maxH) GAUGE-HEIGHT) "solid" "red")
                     0
                     GAUGE-HEIGHT
                     "left"
                     "bottom"
                     SCENE2))

; HappinessState -> HappinessState
; decreases the happiness on every tick till minimum
(check-expect (tockH 100) 99.9)
(check-expect (tockH 0) 0)
(check-expect (tockH -1) 0)
(define (tockH h)
  (max (- h HAPPINESS-DECREMENT) MIN-HAPPINESS))

; HappinessState KeyEvent -> HappinessState
; changes the happiness score by a fixed amount depending on what arrow key is pressed
(check-expect (handleKeyPress 1 "up") (+ 1 HAPPINESS-INCREASE))
(check-expect (handleKeyPress 1 "down") (- 1 HAPPINESS-DECREASE))
(check-expect (handleKeyPress maxH "up") maxH)
(check-expect (handleKeyPress 0 "down") 0)
(define (handleKeyPress h event)
  (cond
    [(key=? event "up") (min (+ h HAPPINESS-INCREASE) 10)]
    [(key=? event "down") (max (- h HAPPINESS-DECREASE) 0)]
    [else h]))

; Number -> HappinessState
; Starts the "happiness gauge" program with max set from the arg
(define (gauge-prog maxHappiness)
  (big-bang maxHappiness [on-tick tockH] [on-key handleKeyPress] [to-draw renderGauge]))

(gauge-prog maxH)
