;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 106-107_virtual_animals_and_zoo) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

; 106 Design the cat-cham world program with VAnimal
; It remains impossible to change the color of a cat or to pet a chameleon.

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

(define CHAM (bitmap "../cham.png"))
(define CHAM-WIDTH (image-width CHAM))
(define CHAM-SCENE-HEIGHT (+ (image-height CHAM) GAUGE-HEIGHT))
(define CHAM-SCENE (empty-scene SCENE-WIDTH CHAM-SCENE-HEIGHT))
(define HAP-UP-CHAM 2)

; Happiness: Number, a range between HAP-MIN & HAP-MAX
; happiness level of a pet

; Direction: one of 1, -1
; specifies in which direction the cat moves
; 1: forward, -1: backward

; Color is one of: "red", "blue", "green"
; represents the color of the virtual chameleon

; VCat: (make-vcat Number Happiness Direction)
; a virtual pet cat with position x, happiness level hap, moving in direction dir
(define-struct vcat [x hap dir])

; VCham: (make-vcham Number Happiness Color)
; a virtual pet chameleon with position x, happiness level hap, and color
(define-struct vcham [x hap color])

; VAnimal is one of: VCat, VCham
; represents a virtual pet

; Any -> Boolean
; checks if arg a VAnimal
(define (vanimal? arg)
  (cond
    [(vcat? arg) #true]
    [(vcham? arg) #true]
    [else #false]))

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

; VCham -> VCham
; moves the chameleon if it's happy and decreases its happiness on every tick
(define (updateCham vcham)
  (make-vcham (if (> (vcham-hap vcham) 0)
                  (modulo (+ (vcham-x vcham) PX-PER-TICK) (- SCENE-WIDTH CHAM-WIDTH))
                  (vcham-x vcham))
              (max (- (vcham-hap vcham) HAP-DECR) HAP-MIN)
              (vcham-color vcham)))

; VCat KeyEvent -> VCat
; changes the pet's happiness score by a fixed amount depending on which arrow key is pressed
(check-expect (handleKeyCat (make-vcat 0 HAP-MIN 1) "up") (make-vcat 0 (+ HAP-MIN HAP-UP) 1))
(check-expect (handleKeyCat (make-vcat 0 HAP-MAX -1) "up") (make-vcat 0 HAP-MAX -1))
(check-expect (handleKeyCat (make-vcat 1 HAP-MIN 1) "down") (make-vcat 1 HAP-MIN 1))
(check-expect (handleKeyCat (make-vcat 1 HAP-MAX -1) "down") (make-vcat 1 (- HAP-MAX HAP-DOWN) -1))
(define (handleKeyCat vcat key)
  (cond
    [(key=? key "up")
     (make-vcat (vcat-x vcat) (min (+ (vcat-hap vcat) HAP-UP) HAP-MAX) (vcat-dir vcat))]
    [(key=? key "down")
     (make-vcat (vcat-x vcat) (max (- (vcat-hap vcat) HAP-DOWN) HAP-MIN) (vcat-dir vcat))]
    [else vcat]))

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

; AnimalType is one of: "cat", "chameleon"
; represents the type of virtual pet

; AnimalType Number -> VAnimal
; Starts the virtual pet game with the specified pet and its starting position
(define (cat-cham animal position)
  (cond
    [(string=? animal "cat")
     (big-bang (make-vcat position HAP-MAX 1)
               [to-draw renderCat]
               [on-tick updateCat]
               [on-key handleKeyCat]
               [check-with vcat?])]
    [(string=? animal "chameleon")
     (big-bang (make-vcham position HAP-MAX "red")
               [to-draw renderCham]
               [on-tick updateCham]
               [on-key handleKeyCham]
               [check-with vcham?])]))

(cat-cham "cat" 50)
(cat-cham "chameleon" 50)

; ======================================================================

; 107 Design cham-and-cat with both animals. Make a Zoo data definition.
; Add ability to focus an animal.

; Zoo: (make-zoo VCat VCham Number)
; represents a virtual pets game with both animals and the index of a currently focused pet
(define-struct zoo [cat cham focus])

; Zoo -> Image
; renders the cham-and-cat game with both animals
(define (render zoo)
  (above (renderCat (zoo-cat zoo)) (renderCham (zoo-cham zoo))))

; Zoo -> Zoo
; updates both animals' properties on each tick
(define (update zoo)
  (make-zoo (updateCat (zoo-cat zoo)) (updateCham (zoo-cham zoo)) (zoo-focus zoo)))

; Zoo -> Zoo
; allows to focus an animal and interact with it using the keyboard
(define (handleKey zoo key-event)
  (cond
    [(key=? key-event "k") (make-zoo (zoo-cat zoo) (zoo-cham zoo) 0)]
    [(key=? key-event "l") (make-zoo (zoo-cat zoo) (zoo-cham zoo) 1)]
    [(= (zoo-focus zoo) 0)
     (make-zoo (handleKeyCat (zoo-cat zoo) key-event) (zoo-cham zoo) (zoo-focus zoo))]
    [(= (zoo-focus zoo) 1)
     (make-zoo (zoo-cat zoo) (handleKeyCham (zoo-cham zoo) key-event) (zoo-focus zoo))]
    [else zoo]))

(define (cham-and-cat zoo)
  (big-bang zoo [to-draw render] [on-tick update] [on-key handleKey] [check-with zoo?]))

(cham-and-cat (make-zoo (make-vcat 20 HAP-MAX 1) (make-vcham 50 HAP-MAX "red") 0))
