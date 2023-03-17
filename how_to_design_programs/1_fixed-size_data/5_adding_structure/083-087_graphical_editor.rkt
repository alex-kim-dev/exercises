;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 083-087_graphical_editor) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; Graphical Editor
; 83 design function render
; 84 design edit
; 85 define run
; 86 modify edit so it ignores keystrokes if there's no space in the scene

(define-struct editor [pre post])
; Editor: (make-editor String String)
; interpretation: (make-editor s t) describes an editor
; whose visible text is (string-append s t) with
; the cursor displayed between s and t

(define SCENE (empty-scene 200 20))
(define CURSOR (rectangle 2 20 "solid" "red"))

; String -> Image
; draws the text
(define (place-text str)
  (text str 16 "black"))

(define (str-first str)
  (if (string=? str "") "" (substring str 0 1)))
(define (str-last str)
  (if (string=? str "") "" (substring str (- (string-length str) 1))))
(define (str-not-first str)
  (if (string=? str "") "" (substring str 1)))
(define (str-not-last str)
  (if (string=? str "") "" (substring str 0 (- (string-length str) 1))))

; Editor -> Image
; draws the text with the cursor
(define (draw-text editor)
  (beside (place-text (editor-pre editor)) CURSOR (place-text (editor-post editor))))

; Editor -> Image
; draw the text and the cursor within the scene
(define (render editor)
  (overlay/align "left" "center" (draw-text editor) SCENE))

; Editor 1String -> Editor
; inserts a char at cursor position
(check-expect (insert-char (make-editor "ab" "de") "c") (make-editor "abc" "de"))
(check-expect (insert-char (make-editor "a" "c") "b") (make-editor "ab" "c"))
(check-expect (insert-char (make-editor "" "b") "a") (make-editor "a" "b"))
(check-expect (insert-char (make-editor "a" "") "b") (make-editor "ab" ""))
(check-expect (insert-char (make-editor "" "") "a") (make-editor "a" ""))
(define (insert-char editor char)
  (make-editor (string-append (editor-pre editor) char) (editor-post editor)))

; Editor -> Editor
; deletes a char to left of the cursor
(check-expect (delete-prev-char (make-editor "abc" "")) (make-editor "ab" ""))
(check-expect (delete-prev-char (make-editor "ab" "cd")) (make-editor "a" "cd"))
(check-expect (delete-prev-char (make-editor "a" "b")) (make-editor "" "b"))
(check-expect (delete-prev-char (make-editor "" "a")) (make-editor "" "a"))
(define (delete-prev-char editor)
  (make-editor (if (string=? (editor-pre editor) "")
                   ""
                   (substring (editor-pre editor) 0 (- (string-length (editor-pre editor)) 1)))
               (editor-post editor)))

; Direction is one of: "left", "right"
; interpretation: 2 directions

; Editor Direction -> Editor
; moves the cursor one character to the specified side
(check-expect (move-cursor (make-editor "ab" "") "left") (make-editor "a" "b"))
(check-expect (move-cursor (make-editor "a" "b") "left") (make-editor "" "ab"))
(check-expect (move-cursor (make-editor "" "ab") "left") (make-editor "" "ab"))
(check-expect (move-cursor (make-editor "" "ab") "right") (make-editor "a" "b"))
(check-expect (move-cursor (make-editor "a" "b") "right") (make-editor "ab" ""))
(check-expect (move-cursor (make-editor "ab" "") "right") (make-editor "ab" ""))
(define (move-cursor editor direction)
  (cond
    [(string=? direction "left")
     (make-editor (str-not-last (editor-pre editor))
                  (string-append (str-last (editor-pre editor)) (editor-post editor)))]
    [(string=? direction "right")
     (make-editor (string-append (editor-pre editor) (str-first (editor-post editor)))
                  (str-not-first (editor-post editor)))]
    [else editor]))

; Editor KeyEvent -> Editor
; manipulates the text and the cursor position depending on which key is pressed
(check-expect (edit (make-editor "a" "c") "b") (make-editor "ab" "c"))
(check-expect (edit (make-editor "a" "b") "\b") (make-editor "" "b"))
(check-expect (edit (make-editor "a" "b") "left") (make-editor "" "ab"))
(check-expect (edit (make-editor "a" "b") "right") (make-editor "ab" ""))
(check-expect (edit (make-editor "a" "b") "\t") (make-editor "a" "b"))
(check-expect (edit (make-editor "a" "b") "\r") (make-editor "a" "b"))
(check-expect (edit (make-editor "a" "b") " ") (make-editor "a " "b"))
(define (edit editor key-event)
  (cond
    [(or (and (= (string-length key-event) 1) (string>=? key-event "a") (string<=? key-event "z"))
         (string=? key-event " "))
     (if (<= (image-width (draw-text (insert-char editor key-event))) (image-width SCENE))
         (insert-char editor key-event)
         editor)]
    [(string=? key-event "\b") (delete-prev-char editor)]
    [(string=? key-event "left") (move-cursor editor key-event)]
    [(string=? key-event "right") (move-cursor editor key-event)]
    [else editor]))

; String -> Editor
; launches an interactive text editor
(define (run initialText)
  (big-bang (make-editor initialText "") [to-draw render] [on-key edit]))

(run "hello")

; 87 redo everything for different data representation - a str and an index

(define-struct editor2 [str i])
; Editor: (make-editor String Number)
; interpretation: (make-editor str i) describes an editor whose visible text is str with the cursor displayed at ith position [0, length of str]

(define (editor2-pre editor)
  (substring (editor2-str editor) 0 (editor2-i editor)))

(define (editor2-post editor)
  (substring (editor2-str editor) (editor2-i editor)))

(define (render2 editor)
  (overlay/align "left" "center" (draw-text-2 editor) SCENE))

(define (draw-text-2 editor)
  (beside (place-text (editor2-pre editor)) CURSOR (place-text (editor2-post editor))))

(define (insert-char-2 editor char)
  (make-editor2 (string-append (editor2-pre editor) char (editor2-post editor))
                (+ (editor2-i editor) 1)))

(define (delete-prev-char-2 editor)
  (make-editor2 (if (string=? (editor2-str editor) "")
                    ""
                    (substring (editor2-str editor) 0 (- (editor2-i editor) 1)))
                (if (= (editor2-i editor) 0) 0 (- (editor2-i editor) 1))))

(define (move-cursor-2 editor direction)
  (cond
    [(string=? direction "left")
     (make-editor2 (editor2-str editor) (if (= (editor2-i editor) 0) 0 (- (editor2-i editor) 1)))]
    [(string=? direction "right")
     (make-editor2 (editor2-str editor)
                   (if (= (editor2-i editor) (string-length (editor2-str editor)))
                       (editor2-i editor)
                       (+ (editor2-i editor) 1)))]))

(define (edit2 editor key-event)
  (cond
    [(or (and (= (string-length key-event) 1) (string>=? key-event "a") (string<=? key-event "z"))
         (string=? key-event " "))
     (if (<= (image-width (draw-text-2 (insert-char-2 editor key-event))) (image-width SCENE))
         (insert-char-2 editor key-event)
         editor)]
    [(string=? key-event "\b") (delete-prev-char-2 editor)]
    [(string=? key-event "left") (move-cursor-2 editor key-event)]
    [(string=? key-event "right") (move-cursor-2 editor key-event)]
    [else editor]))

(define (run2 initialText)
  (big-bang (make-editor2 initialText (string-length initialText)) [to-draw render2] [on-key edit2]))

(run2 "hello")