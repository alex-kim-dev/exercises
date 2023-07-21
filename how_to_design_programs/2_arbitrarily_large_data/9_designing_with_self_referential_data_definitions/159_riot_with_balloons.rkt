;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 159_riot_with_balloons) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

(define (col n img)
  (cond
    [(zero? n) (rectangle (image-width img) 0 "solid" "transparent")]
    [(= n 1) img]
    [else (above img (col (sub1 n) img))]))

(define (row n img)
  (cond
    [(zero? n) (rectangle 0 (image-height img) "solid" "transparent")]
    [(= n 1) img]
    [else (beside img (row (sub1 n) img))]))

(define SQ-SIZE 10)
(define SQUARE (rectangle SQ-SIZE SQ-SIZE "outline" "black"))
(define DOT (circle 3 "solid" "red"))
(define COLUMNS 10)
(define ROWS 20)
(define HALL
  (place-image (col ROWS (row COLUMNS SQUARE))
               (/ (* SQ-SIZE COLUMNS) 2)
               (/ (* SQ-SIZE ROWS) 2)
               (empty-scene (* SQ-SIZE COLUMNS) (* SQ-SIZE ROWS))))

; List-of-posn -> Image
; paints the lecture hall with baloons
(define (add-balloons list)
  (cond
    [(empty? list) HALL]
    [else (underlay/xy (add-balloons (rest list)) (posn-x (first list)) (posn-y (first list)) DOT)]))

; 159 Turn the solution of exercise 153 into a world program. Its main function, dubbed riot, consumes
; how many balloons the students want to throw; its visualization shows one balloon dropping after
; another at a rate of one per second. The function produces the list of Posns where the balloons hit.

(define-struct pair [balloon# lob])
; A Pair is a structure (make-pair N List-of-posns)
; A List-of-posns is one of:
; – '()
; – (cons Posn List-of-posns)
; (make-pair n lob) means n balloons must yet be thrown and added to lob (list of baloons)

; Pair -> Image
; draws the hall with hit baloons
(define (render state)
  (add-balloons (pair-lob state)))

; Pair -> Pair
; decrements the number of baloons and adds a random coordinate to the list where the baloon hit
(define (tock state)
  (if (<= (pair-balloon# state) 0)
      state
      (make-pair (- (pair-balloon# state) 1)
                 (cons (make-posn (random (* COLUMNS SQ-SIZE)) (random (* ROWS SQ-SIZE)))
                       (pair-lob state)))))

; Number -> list of Posns where the balloons hit ???
; consumes the number of baloons to throw and shows the visualization
(define (riot n)
  (big-bang (make-pair n '()) [on-tick tock 1] [to-draw render]))

(riot 5)
