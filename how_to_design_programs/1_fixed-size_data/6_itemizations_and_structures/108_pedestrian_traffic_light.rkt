;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 108_pedestrian_traffic_light) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

(define GO-TIME 10)
(define HURRY-TIME 10)
(define CROSS-TIME (+ GO-TIME HURRY-TIME))
(define TL-RED (bitmap "../pedestrian_traffic_light_red.png"))
(define TL-GREEN (bitmap "../pedestrian_traffic_light_green.png"))

; CrossTime: Number
; represents time left for pedestrians to cross a road

; CrossTime -> Image
; draws the traffic light
(define (render time)
  (place-image
   (cond
     [(>= time HURRY-TIME) TL-GREEN]
     [(>= time 0)
      (overlay (text (number->string time) 44 (if (odd? time) "orange" "green")) TL-GREEN)]
     [(< time 0) TL-RED])
   28
   28
   (empty-scene 56 56)))

; CrossTime -> CrossTime
; decrements the time
(check-expect (tock 10) 9)
(check-expect (tock -1) -1)
(check-expect (tock -2) -1)
(define (tock time)
  (max (- time 1) -1))

; CrossTime KeyEvent -> CrossTime
; starts the countdown of the time to cross a road on spacebar press
(check-expect (handleKey -1 " ") CROSS-TIME)
(check-expect (handleKey -1 "a") -1)
(check-expect (handleKey CROSS-TIME " ") CROSS-TIME)
(check-expect (handleKey 13 " ") 13)
(define (handleKey time key-event)
  (cond
    [(and (key=? key-event " ") (< time 0)) CROSS-TIME]
    [else time]))

; CrossTime -> CrossTime
(define (main initialTime)
  (big-bang initialTime [to-draw render] [on-tick tock 0.5] [on-key handleKey]))

(main -1)
