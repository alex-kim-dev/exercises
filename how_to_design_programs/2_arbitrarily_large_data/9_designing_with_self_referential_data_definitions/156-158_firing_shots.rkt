;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 156-158_firing_shots) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

; Sample Problem Design a world program that simulates firing shots. Every time the “player” hits the
; space bar, the program adds a shot to the bottom of the canvas. These shots rise vertically at the
; rate of one pixel per tick.

; 156 add tests
; 157 make sure the constants are easy to change
; 158 clean up state when shots are not visible on the scene

(define HEIGHT 80) ; distances in terms of pixels
(define WIDTH 100)
(define MIDX (/ WIDTH 2))
(define SCENE (empty-scene WIDTH HEIGHT))
(define SHOT-SIZE 5)
(define SHOT (triangle SHOT-SIZE "solid" "red"))

; Shot: Number, represents the shot's y-coordinate

; List-of-shots is one of:
; – '()
; – (cons Shot List-of-shots)
; the collection of shots fired

; ShotsState: List-of-numbers, each number on such a list represents the y-coordinate of a shot

; ShotsState -> Image
; adds the image of a shot for each y in state at (MID, y) to the scene
(check-expect (render '()) SCENE)
(check-expect (render (cons 9 '())) (place-image SHOT MIDX 9 SCENE))
(check-expect (render (cons 9 (cons (- HEIGHT 5) '())))
              (place-image SHOT MIDX 9 (place-image SHOT MIDX (- HEIGHT 5) SCENE)))
(define (render state)
  (cond
    [(empty? state) SCENE]
    [else (place-image SHOT MIDX (first state) (render (rest state)))]))

; ShotState -> ShotState
; moves each shot in state up by one pixel
(check-expect (tock '()) '())
(check-expect (tock (cons (* -1 SHOT-SIZE) '())) '())
(check-expect (tock (cons 5 (cons 10 '()))) (cons 4 (cons 9 '())))
(check-expect (tock (cons 5 '())) (cons 4 '()))
(define (tock state)
  (cond
    [(empty? state) '()]
    [(<= (first state) (* -1 SHOT-SIZE)) (tock (rest state))]
    [else (cons (sub1 (first state)) (tock (rest state)))]))

; ShotState KeyEvent -> ShotState
; adds a shot to the state if the player presses the space bar
(check-expect (handleKey '() "a") '())
(check-expect (handleKey '() " ") (cons HEIGHT '()))
(check-expect (handleKey (cons 4 '()) " ") (cons HEIGHT (cons 4 '())))
(define (handleKey state key-event)
  (if (key=? key-event " ") (cons HEIGHT state) state))

; ShotState -> ShotState
(define (main initialState)
  (big-bang initialState [on-tick tock] [on-key handleKey] [to-draw render]))

(main '())

; next up 159
