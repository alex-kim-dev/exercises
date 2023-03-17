;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 001-010) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; 1 Cartisian distance from a point to the origin
(define x 3)
(define y 4)
(define (distance x y)
  (sqrt (+ (sqr (- x 0)) (sqr (- y 0)))))
(distance x y)

; 2 Join strings with underscore
(define prefix "hello")
(define suffix "world")
(define (concat str1 str2)
  (string-append str1 "_" str2))
(concat prefix suffix)

; 3 Add underscore at position i
(define str "helloworld")
(define i 5)
(define (insert str pos)
  (string-append (substring str 0 pos) "_" (substring str pos (string-length str))))
(insert str i)

; 4 Delete the ith character
(define (removeChar str pos)
  (string-append (substring str 0 pos) (substring str (+ pos 1) (string-length str))))
(removeChar "hello_world" 5)

; 5 Create an image of a boat or tree, add ability to scale it
(place-image
 (overlay/xy (overlay/xy (rectangle 10 20 "solid" "white") 0 0 (circle 10 "solid" "blue"))
             -10
             10
             (overlay/xy (rectangle 38 10 "solid" "white") -1 0 (ellipse 40 20 "solid" "blue")))
 20
 25
 (empty-scene 40 40))

; 6 Count the number of px in an image
(define cat (bitmap "../cat.png"))
(* (image-width cat) (image-height cat))

; 7 whether sunny is false or friday is true
(define sunny #true)
(define friday #false)
(or (equal? sunny #false) (equal? friday #true))

; 8 Compute whether an image is tall / wide / square
(define (describe img)
  (if (>= (image-height img) (image-width img))
      (if (= (image-height img) (image-width img)) "square" "tall")
      "wide"))
(describe cat)
(describe (rectangle 20 10 "solid" "blue"))
(describe (rectangle 20 20 "solid" "blue"))

; 9 Convert a value to a non-negative number
(define (convert val)
  (cond
    [(string? val) (string-length val)]
    [(image? val) (* (image-height val) (image-width val))]
    [(number? val) (abs val)]
    [(boolean? val) (if (equal? val #true) 10 20)]
    [else val]))
(convert "a") ; length
(convert cat) ; area
(convert -12) ; abs
(convert #true) ; 10
(convert #false) ; 20

; 10 non-code
