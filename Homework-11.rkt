;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Homework-11) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
#|
Accumulator template:

; Domain -> Range 
(define (function d0)
  (local (; Domain AccuDomain -> Range
          ; accumulator ...
          (define (function/a d a)
            ...))
    (function/a d0 a0)))
|#

#|==================== Exercise 4 ====================|#

(define-struct node [left val right])
 
; A NumTree is one of
; - Number
; - (make-node NumTree Number NumTree)
; INTERPRETATION: irrelevant

;product-of-sums: NumTree -> Number
;produces the product of the sums of each path of numbers from the root to each leaf
(define (product-of-sums ntree)
  (local [; p-o-s/a: NumTree Number -> Number
          ; produces the product of the sums of each path of numbers from root to each leaf
          ; accumulator path-acc: the sum of the current path
          ;   from ntree's root to nt as the next potentialleaf
          (define (p-o-s/a nt path-acc)
            (cond [(number? nt) (+ nt path-acc)]
                  [else
                   (* (p-o-s/a (node-left nt) (+ (node-val nt) path-acc))
                      (p-o-s/a (node-right nt) (+ (node-val nt) path-acc)))]))]
    (p-o-s/a ntree 0)))
(check-expect (product-of-sums 2) 2)
(check-expect (product-of-sums (make-node 1 2 3)) 15)
(check-expect (product-of-sums (make-node (make-node 8 1 9) 5 (make-node 4 3 5))) 32760)
(check-expect (product-of-sums (make-node (make-node (make-node 2 8 1) 13 9) 5
                                          (make-node (make-node 4 12 1) 3 (make-node 9 6 7))))
              (* 756 27 504 483))
#|==================== Exercise 5 ====================|#

;to10: [List-of Number] -> Number
; Converts alon0 to it's corresponding base 10 number
(define (to10 alon0)
  (local [; [List-of Number] Number -> Number
          ; Converts alon to a base 10 number
          ; accumulator digits: keeps track of the to10 of alon0 to alon
          (define (to10/a alon digits)
            (cond
              [(empty? alon) digits]
              [else (to10/a (rest alon) (+ (* (first alon) (expt 10 (- (length alon) 1))) digits))]))]
    (to10/a alon0 0)))
(check-expect (to10 '(1 0 2)) 102)
(check-expect (to10 '(3 4 5 6 7)) 34567)
(check-expect (to10 '()) 0)



