;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 109_keys_pattern) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

(define D1 "detecting-first")
(define D2 "detecting-last")
(define COM "complete")
(define ERR "error")

; PState is one of:
; - D1: detecting the first part of the pattern, starting with "a"
; - D2: detecting the last part of the pattern,
; arbitrarily long mix of "b" and "c", and ended by "d"
; - COM: input matches the pattern
; - ERR: input doesn't match the pattern

; PState -> Image
; draws a filled square, each color representing the current state
(define (render state)
  (rectangle 100
             100
             "solid"
             (cond
               [(string=? state D1) "white"]
               [(string=? state D2) "yellow"]
               [(string=? state COM) "green"]
               [(string=? state ERR) "red"])))

; PState KeyEvent -> PState
; listens to a user input to determine the pattern
(check-expect (handleKey D1 "b") ERR)
(check-expect (handleKey D1 "c") ERR)
(check-expect (handleKey D1 "d") ERR)
(check-expect (handleKey D1 " ") ERR)
(check-expect (handleKey D1 "a") D2)
(check-expect (handleKey D2 " ") ERR)
(check-expect (handleKey D2 "a") ERR)
(check-expect (handleKey D2 "b") D2)
(check-expect (handleKey D2 "c") D2)
(check-expect (handleKey D2 "d") COM)
(check-expect (handleKey COM " ") COM)
(check-expect (handleKey ERR " ") ERR)
(define (handleKey state key-event)
  (cond
    [(string=? state D1)
     (cond
       [(key=? key-event "a") D2]
       [else ERR])]
    [(string=? state D2)
     (cond
       [(or (key=? key-event "b") (key=? key-event "c")) D2]
       [(key=? key-event "d") COM]
       [else ERR])]
    [else state]))

; PState -> PState
; starts the program for recognizing the pattern of keys
(define (detect-pattern initialState)
  (big-bang initialState [to-draw render] [on-key handleKey]))

(detect-pattern D1)
