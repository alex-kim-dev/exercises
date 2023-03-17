;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 058-062) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

; 58 Introduce constants that separate the intervals for prices

; A Price falls into one of three intervals:
; — 0 through 1000
; — 1000 through 10000
; — 10000 and above.
; interpretation the price of an item

(define taxBoundary1 1000)
(define taxBoundary2 10000)
(define tax1 0)
(define tax2 0.05)
(define tax3 0.08)

; Price -> Number
; computes the amount of tax charged for p
(check-expect (sales-tax 0) tax1)
(check-expect (sales-tax 537) tax1)
(check-expect (sales-tax taxBoundary1) (* tax2 taxBoundary1))
(check-expect (sales-tax 1282) (* tax2 1282))
(check-expect (sales-tax taxBoundary2) (* tax3 taxBoundary2))
(check-expect (sales-tax 12017) (* tax3 12017))
(define (sales-tax p)
  (cond
    [(and (<= 0 p) (< p taxBoundary1)) tax1]
    [(and (<= taxBoundary1 p) (< p taxBoundary2)) (* tax2 p)]
    [(>= p taxBoundary2) (* tax3 p)]))

; 59 Finish the design of traffic light sim
; 60 Add an alternative tl-next based on numbers, test
; 61 Use constants for N-TrafficLight

; A TrafficLight is one of: "red", "yellow", "green"

; An N-TrafficLight is one of:
; – 0 interpretation the traffic light shows red
; – 1 interpretation the traffic light shows green
; – 2 interpretation the traffic light shows yellow

; An S-TrafficLight is one of:
; – RED
; – GREEN
; – YELLOW

(define RED 0)
(define GREEN 1)
(define YELLOW 2)

; TrafficLight -> TrafficLight
; yields the next state, given current state cs
(check-expect (tl-next 1) "red")
(check-expect (tl-next "green") "yellow")
(check-expect (tl-next "yellow") "red")
(check-expect (tl-next "red") "green")
(define (tl-next state)
  (cond
    [(equal? state "red") "green"]
    [(equal? state "green") "yellow"]
    [else "red"]))

; N-TrafficLight -> N-TrafficLight
; yields the next state, given current state cs
(check-expect (tl-next-numeric 0) 1)
(check-expect (tl-next-numeric 1) 2)
(check-expect (tl-next-numeric 2) 0)
(check-expect (tl-next-numeric 6) 1)
(define (tl-next-numeric state)
  (modulo (+ state 1) 3))

; S-TrafficLight -> S-TrafficLight
; yields the next state, given current state cs
(check-expect (tl-next-symbolic RED) GREEN)
(check-expect (tl-next-symbolic GREEN) YELLOW)
(check-expect (tl-next-symbolic YELLOW) RED)
(define (tl-next-symbolic state)
  (cond
    [(equal? state RED) GREEN]
    [(equal? state GREEN) YELLOW]
    [(equal? state YELLOW) RED]))

; TrafficLight -> Image
; renders the current state cs as an image
(define (tl-render state)
  (place-images (list (circle 10 (if (string=? state "red") "solid" "outline") "red")
                      (circle 10 (if (string=? state "yellow") "solid" "outline") "yellow")
                      (circle 10 (if (string=? state "green") "solid" "outline") "green"))
                (list (make-posn 15 15) (make-posn 45 15) (make-posn 75 15))
                (empty-scene 90 30)))

; TrafficLight -> TrafficLight
; simulates a clock-based American traffic light
(define (traffic-light-sim initial-state)
  (big-bang initial-state [to-draw tl-render] [on-tick tl-next 1]))

(traffic-light-sim "green")

; 62 Door sim

; fsm chart
; "locked": unlock with "u"
; "closed": push with "space" to open / lock with "l"
; "open": locks after some time

; A DoorState is one of:
; – LOCKED
; – CLOSED
; – OPEN

(define LOCKED "locked")
(define CLOSED "closed")
(define OPEN "open")

; DoorState -> DoorState
; closes an open door over the period of one tick
(check-expect (door-close LOCKED) LOCKED)
(check-expect (door-close CLOSED) CLOSED)
(check-expect (door-close OPEN) CLOSED)
(define (door-close state)
  (cond
    [(string=? LOCKED state) LOCKED]
    [(string=? CLOSED state) CLOSED]
    [(string=? OPEN state) CLOSED]))

; DoorState -> DoorState
; changes the state of the door based on the action
(check-expect (door-action LOCKED "u") CLOSED)
(check-expect (door-action CLOSED "l") LOCKED)
(check-expect (door-action CLOSED " ") OPEN)
(check-expect (door-action OPEN "a") OPEN)
(check-expect (door-action CLOSED "a") CLOSED)
(define (door-action state key)
  (cond
    [(and (string=? LOCKED state) (string=? "u" key)) CLOSED]
    [(and (string=? CLOSED state) (string=? "l" key)) LOCKED]
    [(and (string=? CLOSED state) (string=? " " key)) OPEN]
    [else state]))

; DoorState -> Image
; translates the state into a large text image
(check-expect (door-render CLOSED) (text CLOSED 40 "red"))
(define (door-render state)
  (text state 40 "red"))

; DoorState -> DoorState
; simulates a door with an automatic door closer
(define (door-sim initial-state)
  (big-bang initial-state [on-tick door-close 3] [on-key door-action] [to-draw door-render]))

(door-sim LOCKED)