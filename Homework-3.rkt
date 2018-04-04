;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Homework-3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

#| -------------------------------- 
   --------- Exercise 5 -----------
   -------------------------------- |#


(define-struct light [state color])
(define (light-temp l)
  (... (light-state l)
       (light-color l)...))
;; A Light is a ([1, 3] String)
;; *Note: when reading the instructions, we confused the example 
;;  with the actual Exercise, which is why our FSM is called "Light"
;;
;; interpretation: Number[1,3] state defines which state the
;;     Fininte-State-Machine is currently in 
;;     color is a string that represents the color 
;;     that the light is currently in

;;Testing Variable Defines
(define SIZEX 300)
(define SIZEY 300)
(define TEST-LIGHT-S (make-light 1 "white"))
(define TEST-LIGHT-G (make-light 2 "pale green"))
(define TEST-LIGHT-O1 (make-light 3 "spring green"))
(define TEST-LIGHT-O2 (make-light 4 "lime green"))
(define TEST-LIGHT-D (make-light 5 "dark green"))

;;Light-World -> Light-World
;;The universe runner for the Light-World animations
(define (main lw)
  (big-bang lw
    [to-draw draw]
    [on-key update-state]))

;;Light-World KeyEvent -> Light-World
;;Updates the state of the Light-World FSM according to certain key hits
(define (update-state lw key)
  (cond [(= (light-state lw) 5) lw]
        [(string=? key "g") (make-light 2 "pale green")]
        [(string=? key "o") (handle-o lw)]
        [(string=? key "d") (handle-d lw)]
        [else (make-light 1 "white")]))
(check-expect (update-state TEST-LIGHT-S "g") TEST-LIGHT-G)
(check-expect (update-state TEST-LIGHT-G "o") TEST-LIGHT-O1)
(check-expect (update-state TEST-LIGHT-O1 "g") TEST-LIGHT-G)
(check-expect (update-state TEST-LIGHT-O2 "o") TEST-LIGHT-O2)
(check-expect (update-state TEST-LIGHT-O2 "d") TEST-LIGHT-D)
(check-expect (update-state TEST-LIGHT-D "g") TEST-LIGHT-D)
(check-expect (update-state TEST-LIGHT-O1 "r") TEST-LIGHT-S)

;;Light-World -> Image
;;Draws a rectangle, colored based on the given Light-World
(define (draw lw)
  (rectangle SIZEX SIZEY "solid" (light-color lw)))
(check-expect (draw TEST-LIGHT-O2) (rectangle SIZEX SIZEY 
                                              "solid" "lime green"))

;;----------- Helper Methods -----------

;;Light-World -> Light-World
;;Called when the "o" key is pressed
;;moves the FSM forward when the "o" is pressed
;;   if: the FSM has already Started and
;;       The FSM has not ended
(define (handle-o lw)
  (cond [(= (light-state lw) 1) (make-light 1 "white")]
        [(= (light-state lw) 2) (make-light 3 "spring green")]
        [(= (light-state lw) 3) (make-light 4 "lime green")]
        [else lw]))
(check-expect (handle-o TEST-LIGHT-S) TEST-LIGHT-S)
(check-expect (handle-o TEST-LIGHT-O2) TEST-LIGHT-O2)
(check-expect (handle-o TEST-LIGHT-O1) TEST-LIGHT-O2)
(check-expect (handle-o TEST-LIGHT-D) TEST-LIGHT-D)

;;Light-World -> Light-World
;;Called when the "d" key is pressed
;;moves the FSM forward when the "d" is pressed
;;   if: the FSM has not ended
;;       The FSM has gone through the first 4 states
(define (handle-d lw)
  (cond [(= (light-state lw) 4)
         (make-light 5 "dark green")]
        [else (make-light 1 "white")]))
(check-expect (handle-d TEST-LIGHT-O2) TEST-LIGHT-D)
(check-expect (handle-d TEST-LIGHT-O1) TEST-LIGHT-S)

;;--------------------------------------

#| -------------------------------- 
   --------- Exercise 6 -----------
   -------------------------------- |#

;;String -> Posn
;;Converts a valid string to a Position (the first char of the string
;;is the x coordinate and every other char is the y coordinate.
;;If given an invalid string, the function will print an error message
(define (string-to-posn str)
  (if (valid-input str)
      (make-posn (string->number (substring str 0 1)) 
                 (string->number (substring str 1)))
      "decode: bad input string"))
(check-expect (string-to-posn "123") (make-posn 1 23))
(check-expect (string-to-posn "1") "decode: bad input string")
(check-expect (string-to-posn "") "decode: bad input string")

;;String -> Boolean
;;Determines if the given string is valid for converting into a position
;;A String is considered valid if it contains more than 1 charectors
;;(which would be too little convert into a Position)
(define (valid-input str)
  (> (string-length str) 1))
(check-expect (valid-input "0") #false)
(check-expect (valid-input "123") #true)

#| -------------------------------- 
   --------- Exercise 7 -----------
   -------------------------------- |#


;; ----------- Structure Definitions -----------


;; Shape is one of:
;; -- Circle
;; -- Square
;; -- Rectangle

 
(define-struct circl (x y r outline c))
;; A Circle is a (make-circl Number Number Number Boolean Symbol)
;; interpretation: x and y determine the center of the circle,
;;   r the radius, outline whether it's outlined or solid ( true if solid),
;;   and c its color

(define-struct squar (x y size outline c))
;; A Square is a (make-squar Number Number Number Boolean Symbol)
;; interpretation: x and y determine the center of the square,
;;     size is the length of each side of the square,
;;     outline is whether its outlined or solid ( true if solid)
;;     and c is its color

(define-struct recta (x y width height outline c))
;; A Rectangle is a (make-recta Number Number Number Number Boolean Symbol)
;; interpretation: x and y determine the center of the rectangle,
;;     width is the length of the horizontal side of the rectangle,
;;     height is the length of the vertical side of the rectangle,
;;     outline is whether its outlined or solid ( true if solid)
;;     and c is its color

;;Shape -> ?
;;A Shape enumeration
;;interperationa: A Shape is either a Circle Square or Rectangle
;;    They all have coordinates, some kind of size identifier,
;;    A boolean to tell if they are drawn solid or outlined,
;;    And a string to denote color
#|(define (shape-temp sh)
  (cond [(circl? sh)...(circl-x sh)
                    ...(circl-y sh)
                    ...(circl-r sh)
                    ...(circl-outline sh)
                    ...(circl-c sh)...]
        [(squar? sh)...(squar-x sh)
                    ...(squar-y sh)
                    ...(squar-size sh)
                    ...(squar-outline sh)
                    ...(squar-c sh)...]
        [(recta? sh)...(recta-x sh)
                    ...(recta-y sh)
                    ...(recta-width sh)
                    ...(recta-height sh)
                    ...(recta-outline sh)
                    ...(recta-c sh)...]))|#

;; ---------------------------------------------



;; Testing variable defines
(define SCENE (empty-scene SIZEX SIZEY))
(define T-CIRCLE (make-circl  100 100  50 #true "red"))
(define T-SQUARE (make-squar 100 100 50 #true "blue"))
(define T-RECT (make-recta 100 100  50 75 #false "green"))
(define T-POSN1 (make-posn 17 38))
(define T-POSN2 (make-posn 107 93))
(define T-CIRCLE2 (make-circl  100 100  50 #false "red"))
(define T-SQUARE2 (make-squar 100 100 50 #false "blue"))
(define T-RECT2 (make-recta 100 100  50 75 #true "green"))


;;Shape Number -> Shape
;;Returns the given Shape with it's coordinate position 
;;shifted delta pixels in the x-axis
(define (shape-shift-x sh delta)
  (cond [(circl? sh)(make-circl (+ (circl-x sh) delta)
                                (circl-y sh)
                                (circl-r sh)
                                (circl-outline sh)
                                (circl-c sh))]
        [(squar? sh) (make-squar (+ (squar-x sh) delta)
                                 (squar-y sh)
                                 (squar-size sh)
                                 (squar-outline sh)
                                 (squar-c sh))]
        [(recta? sh)(make-recta (+ (recta-x sh) delta)
                                (recta-y sh)
                                (recta-width sh)
                                (recta-height sh)
                                (recta-outline sh)
                                (recta-c sh))]))
(check-expect (shape-shift-x T-CIRCLE 5) 
              (make-circl 105 100 50 #true "red"))
(check-expect (shape-shift-x T-SQUARE 6) 
              (make-squar 106 100 50 #true "blue"))
(check-expect (shape-shift-x T-RECT -7) 
              (make-recta 93 100 50 75 #false "green"))

;;Shape Posn -> Boolean
;;Returns if the given position lies within the given shape
;;(in terms of the Posn's X,Y coordinates
;; and the shape's size and X,Y Coordinates)
(define (shape-in? sh p)
  (cond [(circl? sh)(< (dist-formula (circl-x sh) 
                                     (circl-y sh) p) (circl-r sh))]
        [(squar? sh)(square-in? sh p)]
        [(recta? sh)(rect-in? sh p)]))
(check-expect (shape-in? T-CIRCLE T-POSN1) #false)
(check-expect (shape-in? T-CIRCLE T-POSN2) #true)
(check-expect (shape-in? T-SQUARE T-POSN2) #true)
(check-expect (shape-in? T-RECT T-POSN2) #true)

;;Square Posn -> Boolean
;;Returns if the given position lies within the given square
;;(in terms of the Posn's X,Y coordinates and the square's size 
;; and X,Y Coordinates)
(define (square-in? sq p)
  (and (and (< (posn-x p) (+ (squar-x sq) (/ (squar-size sq) 2)))
            (> (posn-x p)(- (squar-x sq) (/ (squar-size sq) 2))))
       (and (< (posn-y p) (+ (squar-y sq) (/ (squar-size sq) 2)))
            (> (posn-y p) (- (squar-y sq) (/ (squar-size sq) 2))))))
(check-expect (shape-in? T-SQUARE T-POSN2) #true)
(check-expect (shape-in? T-SQUARE T-POSN1) #false)

;;Rectangle Posn -> Boolean
;;Returns if the given position lies within the given rectangle
;;(in terms of the Posn's X,Y coordinates and the rectangle's size
;; and X,Y Coordinates)
(define (rect-in? rect p)
  (and (and (< (posn-x p) (+ (recta-x rect) (/ (recta-height rect) 2))) 
            (> (posn-x p) (- (recta-x rect) (/ (recta-height rect) 2))))
       (and (< (posn-y p) (+ (recta-y rect) (/ (recta-width rect) 2))) 
            (> (posn-y p) (- (recta-y rect) (/ (recta-width rect) 2))))))
(check-expect (shape-in? T-RECT T-POSN2) #true)
(check-expect (shape-in? T-RECT T-POSN1) #false)

;;Shape Scene -> Image
;;draws the given shape onto the given scene,
;;according to the shape's coordinates and size
(define (shape-draw sh sc)
  (place-image (enum-shape-draw sh) (posn-x (get-coordinate sh)) 
               (posn-y (get-coordinate sh)) sc))
(check-expect (shape-draw T-CIRCLE SCENE) 
              (place-image (circle 50 "solid" "red") 100 100 SCENE))
(check-expect (shape-draw T-SQUARE SCENE) 
              (place-image (rectangle 50 50 "solid" "blue") 100 100 SCENE))
(check-expect (shape-draw T-RECT SCENE) 
              (place-image (rectangle 50 75 "outline" "green") 100 100 SCENE))

;;----------- Helper Methods -----------

;;Number Number Posn -> Number
;;A helper method that uses the distance formula to determine
;;how far away a point at x1,y1 would be from Posn p
(define (dist-formula x1 y1 p)
  (sqrt (+ (sqr (- (posn-x p) x1)) (sqr (- (posn-y p) y1)))))
(check-expect (dist-formula 3 4 (make-posn 0 0)) 5)

;;Shape -> Image
;;A Helper Method to get an enumerated Shape 
;;(A Circle, Rectangle, or Square) as an Image
(define (enum-shape-draw sh)
  (cond [(circl? sh)(circle (circl-r sh)
                            (if (circl-outline sh) "solid" "outline")
                            (circl-c sh))]
        [(squar? sh)(rectangle (squar-size sh)
                               (squar-size sh)
                               (if (squar-outline sh) "solid" "outline")
                               (squar-c sh))]
        [(recta? sh)(rectangle(recta-width sh)
                              (recta-height sh)
                              (if (recta-outline sh) "solid" "outline")
                              (recta-c sh))]))
(check-expect (enum-shape-draw T-CIRCLE) (circle 50 "solid" "red"))
(check-expect (enum-shape-draw T-SQUARE) (rectangle 50 50 "solid" "blue"))
(check-expect (enum-shape-draw T-RECT) (rectangle 50 75 "outline" "green"))
(check-expect (enum-shape-draw T-CIRCLE2) (circle 50 "outline" "red"))
(check-expect (enum-shape-draw T-SQUARE2) (rectangle 50 50 "outline" "blue"))
(check-expect (enum-shape-draw T-RECT2) (rectangle 50 75 "solid" "green"))

;;Shape -> Posn
;;A Helper Method to get an enumerated Shape's coordinates as a Posn
(define (get-coordinate sh)
  (cond [(circl? sh)(make-posn (circl-x sh)
                               (circl-y sh))]
        [(squar? sh)(make-posn (squar-x sh)
                               (squar-y sh))]
        [(recta? sh)(make-posn (recta-x sh)
                               (recta-y sh))]))
(check-expect (get-coordinate T-CIRCLE) (make-posn 100 100))
(check-expect (get-coordinate T-SQUARE) (make-posn 100 100))
(check-expect (get-coordinate T-RECT) (make-posn 100 100))

;;--------------------------------------