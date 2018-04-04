;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Homework-5) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Excercise 5
; A Size is one of:
; - "small"
; - "medium"
; - "large"
 
(define-struct drip-coffee [cream size])
(define-struct latte [size])
(define-struct cortado [size])
; A Coffee is one of:
; - (make-drip-coffee Boolean Size)
; - (make-latte Size)
; - (make-cortado Size)
; INTERPRETATION: Represents three possible coffee orders.  Each order 
; has a size; drip coffee might also have cream in it.
#; (define (coff-temp coff)
     (cond
       [(drip-coffee? coff) (... (drip-coffee-cream coff)
                                 (drip-coffee-size coff))]
       [(latte? coff) (... (latte-size coff))]
       [(cortado? coff) (... (cortado-size coff))]))
(define COFFEE1 (make-drip-coffee #true "small"))
(define COFFEE2 (make-drip-coffee #false "medium"))

(define COFFEE3 (make-latte "large"))
(define COFFEE4 (make-latte "medium"))

(define COFFEE5 (make-cortado "medium"))
(define COFFEE6 (make-cortado "small"))
 
; A CoffeeOrder is a List-of-Coffee
; INTERPRETATION: The list of coffee orders at a local coffee shop
#; (define (co-temp co)
     (cond [(empty? co) ...]
           [(cons? co) (co-temp ... (first co)
                                (rest co))]))
(define CO1 (list COFFEE1 COFFEE2))
(define CO2 (list COFFEE3 COFFEE4))
(define CO3 (list COFFEE1 COFFEE2 COFFEE3 COFFEE4 COFFEE5 COFFEE6))

 
; A MaybeCoffee is one of
; - #false
; - Coffee
; INTERPRETATION: Represents maybe having a Coffee

;; CoffeeOrder -> MaybeCoffee
;; When given an order of several coffees, return only the last latte in the order
(define (last-latte co)
  (cond
    [(empty? co) #false]
    [(cons? co)
     (if (latte? (last-latte (rest co)))
         (last-latte (rest co))
         (check-coffee (first co)))]))
(check-expect (last-latte CO2) COFFEE4)

;;Coffee -> MaybeCoffee
;; Determines whether or not a coffee is a latte, if it is not returns #false.
(define (check-coffee coff)
  (if (latte? coff)
      coff
      #false))
(check-expect (check-coffee COFFEE3) (make-latte "large"))
(check-expect (check-coffee COFFEE1) #false)



;;Exercise 6

; NELoImg (Non-Empty List-of-Image) is one of: 
; – (cons Image '()) 
; – (cons Image NELoImg)

(define circ1 (circle 40 "outline" "red"))
(define star1 (star-polygon 45 5 2 "solid" "blue"))
(define square1 (rectangle 80 80 "solid" "green"))
(define rect1 (rectangle 90 80 "solid" "red"))
(define ellipse1(ellipse 40 70 "outline" "light blue"))
(define tri1 (triangle 80 "solid" "yellow"))
(define LIST1 (cons tri1
                    (cons ellipse1
                          (cons circ1
                                (cons star1
                                      (cons square1
                                            (cons rect1 '())))))))
 
(define-struct cr (index images))
; A CR (Camera Roll) is a (make-cr NaturalNumber NELoImg)
; intepretation: 
;  The index represents the position of the current image.
;  The index must be between 0 and the number of images in NELoImg.
(define CR1 (make-cr 3 LIST1))
(define CR2 (make-cr 1 LIST1))
(define CR3 (make-cr 6 LIST1))

;;CR-World--> Image
;;Runs the big-bang function for CR-Worlds
(define (main crw)
  (big-bang crw
    (to-draw display-photo)
    (on-key change-photo)))


;; CR-World --> Image
;; Draws the image from the given CR that is at the current index
;; (the current index is also defined by the given CR)
(define (display-photo crw)
  (get-photo (cr-images crw) 0 (cr-index crw)))
(check-expect (display-photo CR1) circ1)


;; NELoImages NaturalNumber NaturalNumber --> Image
;; Draws the image from the given NELoImg that is at the given desired index
;; interp. the in parameter is the tracker index that we use to go through the image,
;; one image at a time
(define (get-photo loi in des)
  (cond
    [(empty? loi) (empty-scene 0 0)]
    [else (if (= in (- des 1))
              (first loi)
              (get-photo (rest loi) (add1 in) des))]))
(check-expect (get-photo LIST1 0 3) circ1)
(check-expect (get-photo LIST1 10 3) (empty-scene 0 0))


;;CR-World --> CR-World
;;Increments/Decrements the index of the given CR according to left or right keys
(define (change-photo crw key)
  (cond
    [(key=? key "left") (dec-cr crw)]
    [(key=? key "right") (inc-cr crw)]
    [else crw]))
(check-expect (change-photo CR1 "left") (make-cr 2 (cr-images CR1)))
(check-expect (change-photo CR1 "right") (make-cr 4 (cr-images CR1)))
(check-expect (change-photo CR1 "q") CR1)

;;CR-World --> CR-World
;;Increments the index of the given CR, bounded by the size
;;size of the CR's images list
(define (inc-cr crw)
  (if (<= (length (cr-images crw)) (cr-index crw))
      crw
      (make-cr (add1 (cr-index crw)) (cr-images crw))))
(check-expect (inc-cr CR1) (make-cr 4 (cr-images CR1)))
(check-expect (inc-cr CR3) CR3)

;;CR-World --> CR-World
;;Decrements the index of the given CR, bounded by 0
(define (dec-cr crw)
  (if (>= 1 (cr-index crw))
      crw
      (make-cr (sub1 (cr-index crw)) (cr-images crw))))
(check-expect (dec-cr CR1) (make-cr 2 (cr-images CR1)))
(check-expect (dec-cr CR2) CR2)