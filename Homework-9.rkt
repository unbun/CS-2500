;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hw9-1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; ================= Exercise 1 =================

;; -------------- Data Definitions --------------

;An OrderedPair is one of;
; - (list X X)
; - (list OrderedPair X)
;interp: allows us to wrap a list of an
;        unknown amount of X's in order

;; -------------- Functions --------------

;cartesian-product: [List-of [List-of X]] -> [List-of [List-of X]]
;Finds the cartesian product of the lists of lol and lists them in order
(define (cartesian-product lol)
  (map flatten (list-products lol)))
(check-expect (cartesian-product '((a b) (red green blue) (hutt putt)))
              '((a red hutt)
                (a red putt)
                (a green hutt)
                (a green putt)
                (a blue hutt)
                (a blue putt)
                (b red hutt)
                (b red putt)
                (b green hutt)
                (b green putt)
                (b blue hutt)
                (b blue putt)))
(check-expect (cartesian-product '((#f #t) (a b c)))
              '((#false a) (#false b) (#false c) (#true a) (#true b) (#true c)))
(check-expect (cartesian-product '((a))) '())
(check-expect (cartesian-product '()) '())

;flatten: [List-of OrderedPair] -> [List-of X]
;Unwraps a list of OrderedPair into a list of list = number of X's in the Ordered pair
(define (flatten op)
  (cond [(empty? op) '()]
        [(list? (first op)) (append (flatten (first op)) (flatten (rest op)))]
        [else (cons (first op) (flatten (rest op)))]))
(check-expect (flatten (list 1 2)) (list 1 2))
(check-expect (flatten (list 1 (list 'a #false))) (list 1 'a #false))
(check-expect (flatten (list 1 (list 2 (list 3 (list 4 5))))) (list 1 2 3 4 5))


;list-products: [List-of [List-of X]] -> [List-of OrderedPair]
;wraps the cartesian product of the product of lists of lol into an Ordered Pair
;if the list contains too little lists to find a product, returns an empty list
(define (list-products lol)
  (cond [(< (length lol) 2) '()] ;;need to check to use first and rest in the abstraction 
        [(cons? lol) (foldl cart-of-2 (first lol) (rest lol))]))
(check-expect (list-products '((#f #t) (a b c)))
              '((#false a) (#false b) (#false c) (#true a) (#true b) (#true c)))
(check-expect (list-products '((#f #t) (a b c) (1 2 3)))
              (list
               (list (list #false 'a) 1)
               (list (list #false 'a) 2)
               (list (list #false 'a) 3)
               (list (list #false 'b) 1)
               (list (list #false 'b) 2)
               (list (list #false 'b) 3)
               (list (list #false 'c) 1)
               (list (list #false 'c) 2)
               (list (list #false 'c) 3)
               (list (list #true 'a) 1)
               (list (list #true 'a) 2)
               (list (list #true 'a) 3)
               (list (list #true 'b) 1)
               (list (list #true 'b) 2)
               (list (list #true 'b) 3)
               (list (list #true 'c) 1)
               (list (list #true 'c) 2)
               (list (list #true 'c) 3)))
(check-expect (list-products '()) '())
(check-expect  (list-products '((a))) '())


;cart-of-2: [NEList-of X] [NEList-of X] -> [List-of [List-of X]]
;finds the Cartesian product of the 2 lists (un-ordered)
(define (cart-of-2 l2 l1)
  (foldr 
   (λ  (x tail) 
     (append 
      (foldr
       (λ  (y el) (cons (list x y) el)) 
       '() l2) tail))
   '() l1))
(check-expect (cart-of-2 '(a b c) '(1 2)) (list (list 1 'a)
                                                (list 1 'b)
                                                (list 1 'c)
                                                (list 2 'a)
                                                (list 2 'b)
                                                (list 2 'c)))
(check-expect (cart-of-2 '(#f #t 1 2 3 4) '(apple)) (list (list 'apple #false)
                                                          (list 'apple #true)
                                                          (list 'apple 1)
                                                          (list 'apple 2)
                                                          (list 'apple 3)

                                                          (list 'apple 4)))


;; ================= Exercise 2 =================

(define-struct node [left right])

;
(define (all-leafy-trees n)
  (cond [(> 1 n) (list 'leaf)]
        [else  (reverse (remove-all-multi (all-leafy-trees (sub1 n))
                                          (all-trees-less-than-n n)))]))
(check-expect (all-leafy-trees 0) (list 'leaf))
(check-expect (all-leafy-trees 1)
              (list (make-node 'leaf 'leaf)))
(check-expect (all-leafy-trees 2)
              (list
               (make-node (make-node 'leaf 'leaf) 'leaf)
               (make-node 'leaf (make-node 'leaf 'leaf))
               (make-node (make-node 'leaf 'leaf) (make-node 'leaf 'leaf))))
(check-expect (length (all-leafy-trees 3)) 21)

;[List-of X] [List-of X] -> [List-of X]
;removes every instance of every element of the first list
;from the second list
(define (remove-all-multi to-remove l)
  [cond [(empty? to-remove) l]
        [else (remove-all-multi (rest to-remove)
                                (remove-all (first to-remove) l))]])
(check-expect (remove-all-multi '() '()) '())
(check-expect (remove-all-multi '(1 2 3) '(1 2 3 4 5)) '(4 5))

;Number -> [List-of LBT]
;Creates a list of every LBT where
; its's height <= n
(define (all-trees-less-than-n n)
  (cond [(zero? n) (list 'leaf)]
        [(= 1 n) (list (make-node 'leaf 'leaf))]
        [else (cross-lot (append (all-trees-less-than-n (sub1 n))
                                 (all-trees-less-than-n (- n 2))))]))
(check-expect (all-trees-less-than-n 2)
              (list  (make-node (make-node 'leaf 'leaf) (make-node 'leaf 'leaf))
                     (make-node 'leaf (make-node 'leaf 'leaf))
                     (make-node (make-node 'leaf 'leaf) 'leaf)
                     (make-node 'leaf 'leaf)))
(check-expect (all-trees-less-than-n 0) (list 'leaf))

                
  
;[List-of LBT] -> [List-of LBT]
;crosses a list of trees onto itself to create the 2 branches
(define (cross-lot l)
  (cross-2-trees l l))
(check-expect (cross-lot (list (make-node 'leaf 'leaf)))
              (list (make-node (make-node 'leaf 'leaf)
                               (make-node 'leaf 'leaf))))

;[List-of LBT] [List-of LBT] -> [List-of LBT]
;returns the cross-product of the two lists
(define (cross-2-trees list1 list2)
  (cond [(empty? list1) '()]
        [else (append (cross-tree (first list1) list2)
                      (cross-2-trees (rest list1) list2))]))
(check-expect (cross-2-trees (list 'leaf) (list 'leaf)) (list (make-node 'leaf 'leaf)))
                         
;LBT [List-of LBT] -> [List-of LBT]
;attches an LBT onto every LBT in the given list
(define (cross-tree lbt mlist)
  (map (λ(el) (make-node el lbt)) mlist))
  






