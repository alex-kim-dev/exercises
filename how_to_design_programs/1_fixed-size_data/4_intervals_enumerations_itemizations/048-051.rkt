;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 048-051) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; 48 Use stepper
(cond
  [(<= 0 3 10) "bronze"]
  [(and (< 10 3) (<= 3 20)) "silver"]
  [else "gold"])

; 49 Reformulate create-rocket-scene.v5 to use a nested expression
(define (create-rocket-scene h)
  (place-image (circle 20 "solid" "blue")
               50
               (cond
                 [(<= h 40) h]
                 [else 40])
               (empty-scene 100 60)))

; 50 add tests to cover all cases
(check-expect (traffic-light-next "red") "green")
(check-expect (traffic-light-next "green") "yellow")
(check-expect (traffic-light-next "yellow") "red")
(define (traffic-light-next s)
  (cond
    [(string=? "red" s) "green"]
    [(string=? "green" s) "yellow"]
    [(string=? "yellow" s) "red"]))

; 51 Design a big-bang program that simulates a traffic light for a given duration, it changes state on every clock tick.

(define CLOCK-RATE 1)
(define CLOCK-START 0)
(define LIGHT-RADIUS 25)
(define SCENE-SIZE (* LIGHT-RADIUS 2))
(define SCENE (empty-scene SCENE-SIZE SCENE-SIZE))

; ClockState -> Image
; draws the traffic light with a color depending on clock time
(define (renderTrafficLight time)
  (place-image (circle LIGHT-RADIUS
                       "solid"
                       (cond
                         [(= (modulo time 3) 0) "green"]
                         [(= (modulo time 3) 1) "yellow"]
                         [else "red"]))
               LIGHT-RADIUS
               LIGHT-RADIUS
               SCENE))

; ClockState -> ClockState
; increments clock time
(check-expect (tock 0) 1)
(define (tock time)
  (+ time 1))

(define (trafficLight duration)
  (big-bang CLOCK-START [on-tick tock CLOCK-RATE duration] [to-draw renderTrafficLight]))

(trafficLight 11)