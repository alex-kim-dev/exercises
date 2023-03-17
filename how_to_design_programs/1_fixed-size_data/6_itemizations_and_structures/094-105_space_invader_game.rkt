;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 094-105_space_invader_game) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

; Space invader game
; 94 define images, scene
; 95-96 non-code
; 97 Design the functions tank-render, ufo-render, and missile-render
; 98 Design the function si-game-over? & si-render-final
; 99 Design si-move. This function is called for every clock tick to determine to which position the objects move now
; 100 Design the function si-control
; 101 Tests
; 102 Redesign for SIGS.v2 data definition
; 103-105 non-code

(define SCENE-W 200)
(define SCENE-H 200)
(define SCENE (empty-scene SCENE-W SCENE-H))
(define TANK-W 40)
(define TANK-H 15)
(define TANK (rectangle TANK-W TANK-H "solid" "blue"))
(define MISSILE-SIZE 12)
(define MISSILE-VEL 2)
(define MISSILE (triangle MISSILE-SIZE "solid" "red"))
(define UFO
  (overlay (rectangle TANK-W (/ TANK-H 3) "solid" "green") (circle (/ TANK-H 1.75) "solid" "green")))
(define UFO-W (image-width UFO))
(define UFO-H (image-height UFO))

; Range: (make-rng Number Number)
; a range between 2 numbers
(define-struct rng [min max])

(define UFO-XR (make-rng (+ 0 (/ UFO-W 2)) (- SCENE-W (/ UFO-W 2))))
(define UFO-YR (make-rng (+ 0 (/ UFO-H 2)) (- SCENE-H (/ UFO-H 2))))
(define TANK-R (make-rng (+ 0 (/ TANK-W 2)) (- SCENE-W (/ TANK-W 2))))

; UFO: Posn
; UFO's location (the top-down, left-to-right convention)

; Missile: Posn
; missile's location

; Tank: (make-tank Number Number)
; (make-tank x dx) specifies the position (x, HEIGHT)
; and the tank's speed: dx pixels/tick
(define-struct tank [x vel])

(define-struct aim [ufo tank])
(define-struct fired [ufo tank missile])

; State is one of:
; – (make-aim UFO Tank)
; – (make-fired UFO Tank Missile)
; represents the complete state of a space invader game

; SELECTORS ===========================================

; State -> Number, returns tank's x location
(define (get-tank-x state)
  (cond
    [(aim? state) (tank-x (aim-tank state))]
    [(fired? state) (tank-x (fired-tank state))]))

; State -> Number, returns tank's velocity
(define (get-tank-vel state)
  (cond
    [(aim? state) (tank-vel (aim-tank state))]
    [(fired? state) (tank-vel (fired-tank state))]))

; State -> Number, returns ufo's x coordinate
(define (get-ufo-x state)
  (cond
    [(aim? state) (posn-x (aim-ufo state))]
    [(fired? state) (posn-x (fired-ufo state))]))

; State -> Number, returns ufo's y coordinate
(define (get-ufo-y state)
  (cond
    [(aim? state) (posn-y (aim-ufo state))]
    [(fired? state) (posn-y (fired-ufo state))]))

; State -> Number, returns missile's x coordinate
(define (get-missile-x state)
  (posn-x (fired-missile state)))

; State -> Number, returns missile's y coordinate
(define (get-missile-y state)
  (posn-y (fired-missile state)))

; RENDERING ===========================================

; Tank Image -> Image
; adds tank to the given image
(define (tank-render tank img)
  (place-image TANK (tank-x tank) (- (image-height img) (/ TANK-H 2)) img))

; Missile Image -> Image
; adds missile to the given image
(define (missile-render missile img)
  (place-image MISSILE (posn-x missile) (posn-y missile) img))

; UFO Image -> Image
; adds ufo to the given image
(define (ufo-render ufo img)
  (place-image UFO (posn-x ufo) (posn-y ufo) img))

; State -> Image
; draws TANK, UFO, and possibly MISSILE on the SCENE
(define (si-render state)
  (cond
    [(aim? state) (ufo-render (aim-ufo state) (tank-render (aim-tank state) SCENE))]
    [(fired? state)
     (ufo-render (fired-ufo state)
                 (tank-render (fired-tank state) (missile-render (fired-missile state) SCENE)))]))

; State -> Image
; draws a final scene wth
(define (si-render-final state)
  (overlay (cond
             [(>= (get-ufo-y state) (rng-max UFO-YR)) (text "Game over!" 20 "crimson")]
             [else (text "You win!" 20 "darkgreen")])
           (si-render state)))

; STATE UPDATES =======================================

; Integer Integer -> Integer
; produces a random integer in range [min, max]
(define (rand min max)
  (+ (random (+ (- max min) 1)) min))

; Number Range -> Number
; returns a given number, if it's in the range; min or max of the range if not
(define (clamp n range)
  (min (rng-max range) (max (rng-min range) n)))

; State -> State
; updates the position of objects in the game on every tick
(define (si-move state)
  (cond
    [(aim? state)
     (make-aim (make-posn (clamp (+ (get-ufo-x state) (rand -5 5)) UFO-XR)
                          (clamp (+ (get-ufo-y state) 1) UFO-YR))
               (make-tank (clamp (+ (get-tank-x state) (get-tank-vel state)) TANK-R)
                          (get-tank-vel state)))]
    [(fired? state)
     (make-fired (make-posn (clamp (+ (get-ufo-x state) (rand -5 5)) UFO-XR)
                            (clamp (+ (get-ufo-y state) 1) UFO-YR))
                 (make-tank (get-tank-x state) (get-tank-vel state))
                 (make-posn (get-missile-x state) (- (get-missile-y state) MISSILE-VEL)))]))

; State KeyEvent -> State
; controls the tank's movement direction and fires a missile on key press
(define (si-control state key-event)
  (cond
    [(and (key=? key-event " ") (aim? state))
     (make-fired (make-posn (get-ufo-x state) (get-ufo-y state))
                 (make-tank (get-tank-x state) (get-tank-vel state))
                 (make-posn (get-tank-x state) (- SCENE-H TANK-H (/ MISSILE-SIZE 2))))]
    [(and (aim? state) (or (key=? key-event "left") (key=? key-event "right")))
     (make-aim (make-posn (get-ufo-x state) (get-ufo-y state))
               (make-tank (get-tank-x state)
                          (* (cond
                               [(key=? key-event "left") -1]
                               [(key=? key-event "right") 1])
                             (abs (get-tank-vel state)))))]
    [else state]))

; State -> Boolean
; stops the game if the ufo has landed
(define (si-game-over? state)
  (or (>= (get-ufo-y state) (rng-max UFO-YR))
      (and (fired? state)
           (< (abs (- (get-missile-x state) (get-ufo-x state))) (/ UFO-W 2))
           (< (abs (- (get-missile-y state) (get-ufo-y state))) (/ UFO-H 2)))))

; MAIN ================================================

(define (run-si state)
  (big-bang state
            [to-draw si-render]
            [on-tick si-move]
            [on-key si-control]
            [stop-when si-game-over? si-render-final]))

(run-si (make-aim (make-posn (/ SCENE-W 2) (rng-min UFO-YR)) (make-tank 30 2)))