;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 033-038) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)
(require 2htdp/universe)
(require 2htdp/image)

; 33 Research the “year 2000” problem

; 34 Design the function string-first
; String -> String (Char)
; returns the 1st character of a non-empty string
; given: "hello", expect: "h"
(define (string-first str)
  (substring str 0 1))
(string-first "hello")

; 35 Design the function string-last
; String -> String (Char)
; returns the last character of a non-empty string
; given: "hello", expect: "o"
(define (string-last str)
  (substring str (- (string-length str) 1) (string-length str)))
(string-last "hello")

; 36 Desing the function image-area
; Image -> Number (Px)
; calculates the number of pixels in an image
; given: (rectangle 10 20 "solid" "red"), expect: 200
(define (image-area img)
  (* (image-width img) (image-height img)))
(image-area (rectangle 10 20 "solid" "red"))

; 37 Desing the function string-rest
; String -> String
; returns a passed string without the 1st char
; given: "hello", expect: "ello"
; given: "h", expect: ""
; given: "", expect: ""
(define (string-rest str)
  (if (= (string-length str) 0) "" (substring str 1 (string-length str))))
(string-rest "hello")
(string-rest "h")
(string-rest "")

; 38 Desing the function string-remove-last
; String -> String
; returns a passed string without the last char
; given: "hello", expect: "hell"
; given: "h", expect: ""
; given: "", expect: ""
(define (string-remove-last str)
  (if (= (string-length str) 0) "" (substring str (- (string-length str) 1) (string-length str))))
(string-remove-last "hello")
(string-remove-last "h")
(string-remove-last "")
