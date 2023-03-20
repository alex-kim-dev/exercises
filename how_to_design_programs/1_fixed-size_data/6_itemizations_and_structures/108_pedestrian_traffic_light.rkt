;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 108_pedestrian_traffic_light) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

(define GO 0)
(define HURRY 1)
(define STOP 2)
(define GO-TIME 10)
(define HURRY-TIME 10)
(define CROSS-TIME (+ GO-TIME HURRY-TIME))
(define TL-RED (bitmap "../pedestrian_traffic_light_red.png"))
(define TL-GREEN (bitmap "../pedestrian_traffic_light_green.png"))

; Mode is one of:
; - GO: pedestrians are allowed to cross a road
; - HURRY: the time to cross a road is ending
; - STOP: pedestrians should not cross a road

; Clock is a Number, the internal traffic light clock, which is used to change the state

; State is (make-state Mode Clock)
; represents the complete state of a traffic light
(define-struct tlstate [mode clock])

; State -> Image, draws the traffic light
(define (render state)
  (place-image (cond
                 [(= (tlstate-mode state) GO) TL-GREEN]
                 [(= (tlstate-mode state) HURRY)
                  (overlay (text (number->string (tlstate-clock state))
                                 44
                                 (if (odd? (tlstate-clock state)) "orange" "green"))
                           TL-GREEN)]
                 [(= (tlstate-mode state) STOP) TL-RED])
               28
               28
               (empty-scene 56 56)))

; State -> State, decrements the clock and changes the mode over time
(define (tock state)
  (cond
    [(= (tlstate-mode state) GO)
     (make-tlstate (if (<= (tlstate-clock state) HURRY-TIME) HURRY GO) (- (tlstate-clock state) 1))]
    [(= (tlstate-mode state) HURRY)
     (make-tlstate (if (<= (tlstate-clock state) 0) STOP HURRY) (- (tlstate-clock state) 1))]
    [else state]))

; State KeyEvent -> State, switches the mode to GO on spacebar press
(define (handleKey state key-event)
  (cond
    [(and (key=? key-event " ") (= (tlstate-mode state) STOP)) (make-tlstate GO CROSS-TIME)]
    [else state]))

; Mode -> State
(define (main intialMode)
  (big-bang (make-tlstate intialMode 0) [to-draw render] [on-tick tock 0.5] [on-key handleKey]))

(main STOP)
