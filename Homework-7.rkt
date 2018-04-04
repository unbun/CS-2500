;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Homework-7) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

#|========== Exercise 3 ==========|#


;;A Nelon is a Non-empty List-of Number
;; nelon Template:
#;(define (nelon-temp l)
(cond[(empty? (rest l))...]
[else (if (... (first l)...)
(first l)
(nelon-temp (rest l)))]))

;; combined: [X] [X X -> X] [List-of X] -> X
;; Purpose Statement: combined takes in a list of number and a function,
;; returns a number as a result of teh given function
(define (combined f l)
  (cond
    [(empty? (rest l))
     (first l)]
    [else
     (if (f (first l)
            (combined f (rest l)))
         (first l)
         (combined f (rest l)))]))
(check-expect (combined > (list 25 24 23 22 21 20 19 18 17 16 15 14 13
                                12 11 10 9 8 7 6 5 4 3 2 1)) 25)
(check-expect (combined > (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16
                                17 18 19 20 21 22 23 24 25)) 25)
(check-expect (combined < (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16
                                17 18 19 20 21 22 23 24 25)) 1)
(check-expect (combined < (list 8888  219923 66 345)) 66)

;; inf-1 Nelon -> Number
;; Purpose Statement: determines the smallest number in a non empty list of strings
(define (inf-1 nelon)
  ;; combined: [X] [X X -> X] [List-of X] -> X
  (combined < nelon))

(check-expect (inf-1 (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16
                           17 18 19 20 21 22 23 24 25)) 1)
(check-expect (inf-1 (list 8888  219923 66 345)) 66)
(check-expect (inf-1 (list 25 24 23 22 21 20 19 18 17 16 15 14 13
                           12 11 10 9 8 7 6 5 4 3 2 1)) 1)
(check-expect (inf-1 (list 12 88 604 3029 5 555555)) 5)

;; sub-1: Nelon -> Number
;; Purpose Statement: determines the largest number in a non empty list of strings
(define (sub-1 nelon)
  ;; combined: [X] [X X -> X] [List-of X] -> X
  (combined > nelon))

(check-expect (sub-1 (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16
                           17 18 19 20 21 22 23 24 25)) 25)
(check-expect (sub-1 (list 8888  219923 66 345)) 219923)
(check-expect (sub-1 (list 25 24 23 22 21 20 19 18 17 16 15 14 13
                           12 11 10 9 8 7 6 5 4 3 2 1)) 25)
(check-expect (sub-1 (list 12 88 604 3029 5 555555)) 555555)

;;Abstraction checks one number against the entirety of the rest of the list,
;; and then repeats that process for every sublist possibility.
;;Recursion goes through the entirey of a list before coming up with a result,
;; making it inherntly slow.
;;If you use recusion on abstraction, you are using an inheretly slow process,
;; repeatedly (on each sublist).
;; this is why inf-1 and sub-1 are slow

;; inf-2: Nelon -> Number
;; Purpose Statement: takes in a non-empty list of strings and returns the minimum number
(define (inf-2 nelon)
  ; foldr: [X Y] [X Y -> Y] Y [List-of X] -> Y
  (foldr min (first nelon) nelon))

(check-expect (inf-2 (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16
                           17 18 19 20 21 22 23 24 25)) 1)
(check-expect (inf-2 (list 8888  219923 66 345)) 66)
(check-expect (inf-2 (list 25 24 23 22 21 20 19 18 17 16 15 14 13
                           12 11 10 9 8 7 6 5 4 3 2 1)) 1)
(check-expect (inf-2 (list 12 88 604 3029 5 555555)) 5)

;; sub-2 Nelon -> Number
;; Purpose Statement: takes in a non-empty list of strings and returns the maximum number
(define (sub-2 nelon)
  ; foldr: [X Y] [X Y -> Y] Y [List-of X] -> Y
  (foldr max (first nelon) nelon))

(check-expect (sub-2 (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16
                           17 18 19 20 21 22 23 24 25)) 25)
(check-expect (sub-2 (list 8888  219923 66 345)) 219923)
(check-expect (sub-2 (list 25 24 23 22 21 20 19 18 17 16 15 14 13
                           12 11 10 9 8 7 6 5 4 3 2 1)) 25)
(check-expect (sub-2 (list 12 88 604 3029 5 555555)) 555555)

;;inf-2 and sub-2 do not use recusion, but rather the min/max functions, which
;; only need to look at 2 numbers at a time, rather than whole lists
;; like the recursive functions need to do.
;; Without recursion, the whole thing is sped up.


#|========== Exercise 4 ==========|#

; A [Maybe String] is one of: 
; – #false 
; – String

; A [Maybe [List-of String]] is one of: 
; – #false 
; – [List-of String]
;;Template:
#;(define (maybe-los-temp los)
    (cond [(empty? los) #f]
          [(cons? los) (... (first los)
                            (maybe-los-temp (rest los)))]))

; A [List-of Maybe String] is a a List-of [Maybe String]


;occurs: String [List-of String] -> [Maybe [List-of String]]
; returns the remainder of los starting with s, excluding s
; #false otherwise 
(define (occurs s los)
  (cond [(list? (get-tails s los))
         (rest (get-tails s los))]
        [else #f]))
(check-expect (occurs "a" (list "b" "a" "d" "e"))
              (list "d" "e"))
(check-expect (occurs "a" (list "b" "c" "d")) #f)

;get-tails: String [List-of String] -> [Maybe [List-of String]]
;returns the remainder of los starting with s, including s
;; #false otherwise
(define (get-tails s los)
  (local(
         (define (custom-str=? s1)
           (string=? s s1)))
    (memf custom-str=? los)))
(check-expect (get-tails "a" (list "b" "a" "d" "e"))
              (list "a" "d" "e"))
(check-expect (get-tails "a" (list "b" "c" "d")) #f)