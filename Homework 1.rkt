;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Homework 1_reformated |) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/image)
(require 2htdp/universe)

;;Number -> String
;;removes a given number of characters from the end of the string "QWERT",
;;based on a given table
(define (table number)
  (substring "QWERTY" 0 (max (- 6 number) 0)))
(check-expect (table 1) "QWERT")
(check-expect (table 4) "QW")
(check-expect (table 10) "")

;;Number -> String
;;returns the given number of characters of the string "QWERTY",
;;starting from the beginning of the string
(define (QWERTY number)
  (substring "QWERTY" 0 (min number 6)))
(check-expect (QWERTY 3) "QWE")
(check-expect (QWERTY 13) "QWERTY")
(check-expect (QWERTY 20) "QWERTY")

;;Number Number -> String
;;returns a message to a driver based on his/her speed and the speed limit,
;;warning them if they are too fast or giving them a ticket
(define (ticket car-speed speed-limit)
  (cond [(< car-speed speed-limit) "fine"]
        [(<= car-speed (+ 5 speed-limit)) "danger"]
        [else (string-append "you drove " (number->string car-speed) " mph. You get a ticket!")]))
(check-expect (ticket 29 30) "fine")
(check-expect (ticket 30 30) "danger")
(check-expect (ticket 35 30) "danger")
(check-expect (ticket 36 30) "you drove 36 mph. You get a ticket!")

;variable definitions

(define SIZEX 500)
(define SIZEY 300)
(define FRAME (empty-scene SIZEX SIZEY))

;;World -> World
;;main class of the Hello World animation, showing a text that grows
;;to a set size and can be reset with a mouse click
(define (main w)
  (big-bang w
    [to-draw draw-text]
    [on-tick incr-size]
    [on-mouse reset-size]))

;;Number -> Image
;;draws "Hello World" onto a frame at a given size
(define (draw-text s)
  (place-image (text "Hello World" s "black") (/ SIZEX 2) (/ SIZEY 2) FRAME))
(check-expect (draw-text 10) (place-image (text "Hello World" 10 "black")
                                          (/ SIZEX 2) (/ SIZEY 2) FRAME))

;;World -> World
;;increase the size of text at each tick
(define (incr-size w)
  (min 80 (+ w 1)))
(check-expect (incr-size 50) 51)
(check-expect (incr-size 82) 80)

;;World number number MouseEvent -> World
;;resets the size of the text to 1 when a mouse click is detected
(define (reset-size w x y mouse-event)
  (cond [(string=? mouse-event "button-up") 1]
        [else w]))
(check-expect (reset-size 82 5 5 "button-up") 1)
(check-expect (reset-size 51 10 6 "button-down") 51)




  


