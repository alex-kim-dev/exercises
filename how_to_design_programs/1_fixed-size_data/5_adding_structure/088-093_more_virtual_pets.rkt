;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 088-093_more_virtual_pets) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

; Vitual cat
; 88 Define a structure type that keeps track of the cat’s x-coordinate and its happiness
; 89 Design the happy-cat world program
; 90 Make the cat stop at 0 happiness
; 91 Make the cat change its direction when it reaches either edge

(define CAT1 (bitmap "../cat.png"))
(define CAT2 (bitmap "../cat2.png"))
(define CAT-WIDTH (image-width CAT1))
(define CAT-HEIGHT (image-height CAT1))
(define PX-PER-TICK 3)
(define HAP-DECR 0.1)
(define HAP-DOWN 1/5)
(define HAP-UP 1/3)
(define HAP-MIN 0)
(define HAP-MAX 10)
(define GAUGE-HEIGHT 5)
(define SCENE-HEIGHT (+ CAT-HEIGHT GAUGE-HEIGHT))
(define SCENE-WIDTH 300)
(define SCENE (empty-scene SCENE-WIDTH SCENE-HEIGHT))

; Happiness: Number, a range between HAP-MIN & HAP-MAX
; interpretation: happiness level of a pet

; Direction: one of 1, -1
; interpretation: specifies in which direction the cat moves
; 1: forward, -1: backward

; VCat: (make-vcat Number Happiness Direction)
; interpretation: a virtual pet cat with position x, happiness level hap,
; moving in direction dir
(define-struct vcat [x hap dir])

; VCat -> Image
; draws the cat on the scene at position x, and its happiness level
(define (renderCat vcat)
  (place-images/align
   (list (cond
           [(even? (vcat-x vcat)) CAT1]
           [else CAT2])
         (rectangle (* (/ (vcat-hap vcat) HAP-MAX) SCENE-WIDTH) GAUGE-HEIGHT "solid" "red"))
   (list (make-posn (vcat-x vcat) 0) (make-posn 0 (- SCENE-HEIGHT GAUGE-HEIGHT)))
   "left"
   "top"
   SCENE))

; VCat -> VCat
; moves the cat if it's happy and decreases its happiness on every tick
(check-expect (updateCat (make-vcat 0 HAP-MIN 1)) (make-vcat 0 HAP-MIN 1))
(check-expect (updateCat (make-vcat 5 3 1)) (make-vcat (+ 5 PX-PER-TICK) (- 3 HAP-DECR) 1))
(check-expect (updateCat (make-vcat 7 3 -1)) (make-vcat (- 7 PX-PER-TICK) (- 3 HAP-DECR) -1))
(check-expect (updateCat (make-vcat 0 3 -1)) (make-vcat (- 0 PX-PER-TICK) (- 3 HAP-DECR) 1))
(check-expect (updateCat (make-vcat (- SCENE-WIDTH CAT-WIDTH) 3 1))
              (make-vcat (+ (- SCENE-WIDTH CAT-WIDTH) PX-PER-TICK) (- 3 HAP-DECR) -1))
(define (updateCat vcat)
  (make-vcat
   (+ (vcat-x vcat) (if (> (vcat-hap vcat) HAP-MIN) (* PX-PER-TICK (vcat-dir vcat)) 0))
   (max (- (vcat-hap vcat) HAP-DECR) HAP-MIN)
   (* (vcat-dir vcat)
      (if (and (>= (+ (vcat-x vcat) (* PX-PER-TICK (vcat-dir vcat))) 0)
               (<= (+ (vcat-x vcat) (* PX-PER-TICK (vcat-dir vcat))) (- SCENE-WIDTH CAT-WIDTH)))
          1
          -1))))

; VCat KeyEvent -> VCat
; changes the pet's happiness score by a fixed amount depending on which arrow key is pressed
(check-expect (handleKey (make-vcat 0 HAP-MIN 1) "up") (make-vcat 0 (+ HAP-MIN HAP-UP) 1))
(check-expect (handleKey (make-vcat 0 HAP-MAX -1) "up") (make-vcat 0 HAP-MAX -1))
(check-expect (handleKey (make-vcat 1 HAP-MIN 1) "down") (make-vcat 1 HAP-MIN 1))
(check-expect (handleKey (make-vcat 1 HAP-MAX -1) "down") (make-vcat 1 (- HAP-MAX HAP-DOWN) -1))
(define (handleKey vcat key)
  (cond
    [(key=? key "up")
     (make-vcat (vcat-x vcat) (min (+ (vcat-hap vcat) HAP-UP) HAP-MAX) (vcat-dir vcat))]
    [(key=? key "down")
     (make-vcat (vcat-x vcat) (max (- (vcat-hap vcat) HAP-DOWN) HAP-MIN) (vcat-dir vcat))]
    [else vcat]))

; Happiness -> VCat
; Starts the "happy cat" game with specified maximum happiness level.
(define (happy-cat maxHap)
  (big-bang (make-vcat 0 maxHap 1) [to-draw renderCat] [on-tick updateCat] [on-key handleKey]))

(happy-cat HAP-MAX)

; Vitual chameleon
; 92 Design the cham program
; 93 Add a 3-colored background to the scene

(define CHAM (bitmap "../cham.png"))
(define CHAM-WIDTH (image-width CHAM))
(define CHAM-SCENE-HEIGHT (+ (image-height CHAM) GAUGE-HEIGHT))
(define CHAM-SCENE (empty-scene SCENE-WIDTH CHAM-SCENE-HEIGHT))
(define HAP-UP-CHAM 2)

; Color is one of: "red", "blue", "green"
; represents the color of the virtual chameleon

; VCham: (make-vcham Number Happiness Color)
; interpretation: a virtual pet chameleon with position x, happiness level hap,
; and color
(define-struct vcham [x hap color])

; Color -> Image
; draws the chameleon image filled with specified color
(define (paintCham color)
  (overlay CHAM (rectangle (image-width CHAM) (image-height CHAM) "solid" color)))

; VCham -> Image
; draws the chameleon on the scene at position x with color color,
; and its happiness level
(define (renderCham vcham)
  (place-images/align
   (list (paintCham (vcham-color vcham))
         (rectangle (* (/ (vcham-hap vcham) HAP-MAX) SCENE-WIDTH) GAUGE-HEIGHT "solid" "red"))
   (list (make-posn (vcham-x vcham) 0) (make-posn 0 (- CHAM-SCENE-HEIGHT GAUGE-HEIGHT)))
   "left"
   "top"
   CHAM-SCENE))

; VCham -> VCham
; moves the chameleon if it's happy and decreases its happiness on every tick
(define (updateCham vcham)
  (make-vcham (if (> (vcham-hap vcham) 0)
                  (modulo (+ (vcham-x vcham) PX-PER-TICK) (- SCENE-WIDTH CHAM-WIDTH))
                  (vcham-x vcham))
              (max (- (vcham-hap vcham) HAP-DECR) HAP-MIN)
              (vcham-color vcham)))

; VCham KeyEvent -> VCham
; increases the chameleon happiness on arrow down
(define (handleKeyCham vcham key)
  (make-vcham (vcham-x vcham)
              (cond
                [(key=? key "down") (min (+ (vcham-hap vcham) HAP-UP-CHAM) HAP-MAX)]
                [else (vcham-hap vcham)])
              (cond
                [(key=? key "r") "red"]
                [(key=? key "g") "green"]
                [(key=? key "b") "blue"]
                [else (vcham-color vcham)])))

; Happiness -> VCham
; starts the virtual chameleon game with specified max happiness level
(define (cham maxHap)
  (big-bang (make-vcham 0 maxHap "red")
            [to-draw renderCham]
            [on-tick updateCham]
            [on-key handleKeyCham]))

(cham HAP-MAX)
