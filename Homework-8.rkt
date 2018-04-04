;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hw8) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
(require "lab8-teachpack.rkt") 

;; ---------------- Exercise 1 --------------------

; A Road is one of:
; - 'dead-end
; - (make-straightaway String PositiveNumber Road)
; - Intersection
 
(define-struct straightaway [name distance more])
; INTERPRETATION: A road with some name and some amount of distance
; until the next portion of road
 
; An Intersection is a [List-of Road]


(define r1 'dead-end)
(define r2 (list (make-straightaway "fresh" 33 r1) (make-straightaway "yellow" 44 r1) 'dead-end))
(define r3 (list (make-straightaway "green" 1 r1)
                 (make-straightaway "yellow" 2 r1)
                 (make-straightaway "fresh" 3 r1)
                 (make-straightaway "grrrrr" 4 r1)))
(define r4 (make-straightaway "lala" 22
                              (make-straightaway "father" 44
                                                 (make-straightaway "purple" 44 'dead-end))))
(define r5 (list (make-straightaway "fresh" 33 r1)
                 (make-straightaway "lala" 22
                                    (make-straightaway "father" 44 'dead-end))))
(define r6 (list (make-straightaway "party" 66 r1)
                 (list (make-straightaway "grrrrr" 9 r1)
                       (make-straightaway "lalala" 1100 r1))))

;; total-road-length: road-> number
;; Takes in a road and returns the total length of all its connected roads
(define (total-road-length road)
  (cond[(equal? 'dead-end road) 0]
       [(straightaway? road) (+ (straightaway-distance road)
                                (total-road-length (straightaway-more road)))]
       [else (intersection-length road)]))

(check-expect (total-road-length r1) 0)
(check-expect (total-road-length r2) 77)
(check-expect (total-road-length r3) 10)
(check-expect (total-road-length r4) 110)

;; intersection-length: Intersecion--> Number
;; Takes in an intersection and returns the total length
(define (intersection-length int)
  (foldr sum-dist 0 int))

(check-expect (intersection-length r2) 77)
(check-expect (intersection-length r3) 10)

;;sum-dist: Intersection Number--> Number
(define (sum-dist int n)
  (cond [(equal? 'dead-end int) 0]
        [(straightaway? int) (+ (straightaway-distance int) n)]
        [else (intersection-length int)]))

(check-expect (sum-dist r2 0) 77)
(check-expect (sum-dist r3 0) 10)

;--------------------------------Exercise 2--------------------------------;

;; road-names: Road--> [List of Strings]
;; takes in a road and returns a list of road names
(define (road-names road)
  (cond[(equal? 'dead-end road) (list 'dead-end)]
       [(straightaway? road) (cons (straightaway-name road)
                                   (road-names (straightaway-more road)))]
       [else (intersection-names road)]))

(check-expect (road-names r2) (list "fresh" "yellow" 'dead-end))
(check-expect (road-names r3) (list "green" "yellow" "fresh" "grrrrr"))
(check-expect (road-names r1) (list 'dead-end))
(check-expect (road-names r4) (list "lala" "father" "purple" 'dead-end))

;; intersection-names: Intersection--> [List of Strings]
;; takes in an intersection and returns a list of road names
(define (intersection-names int)
  (map inter->string int))

(check-expect (intersection-names r2) (list "fresh" "yellow" 'dead-end))
(check-expect (intersection-names r3) (list "green" "yellow" "fresh" "grrrrr"))

;; inter->string: Intersection--> [List of Strings]
;; takes in an intersection and returns a list of road names
(define (inter->string int)
  (cond [(equal? 'dead-end int) 'dead-end]
        [(straightaway? int) (straightaway-name int)]
        [else (intersection-names int)]))

(check-expect (intersection-names r2) (list "fresh" "yellow" 'dead-end))
(check-expect (intersection-names r3) (list "green" "yellow" "fresh" "grrrrr"))
(check-expect (intersection-names r5) (list "fresh" "lala"))
(check-expect (intersection-names r6) (list "party" (list "grrrrr" "lalala")))

;---------------- Exercise 3 ------------------
(require "lab8-teachpack.rkt") 

;StringPair is a (list String JSON)

;**NOTE: To flatten a piece of data means to break it down to a [List-of StringPair],
;  where every JSON in the original data has it's own string as the problem
;  defines it as the first element, and  the second of any element of the list will not
;  be a JSONArray or a JSONObject.
;  When a Purpose Statement uses the verb "flattens", that's what that means


;;JSONPair -> [List-of StringPair]
;flattens the given JSON Pair
(define (flatten-json-pair jp)
  (append-json-string (symbol->string (first jp))
                      (flatten-json (second jp))))
(check-expect (flatten-json-pair tpair-1) (list (list "greeting-formal-STRING" "hello")
                                                (list "greeting-informal-STRING" "howdy")
                                                (list "greeting-garbage-0-BOOLEAN" #true)
                                                (list "greeting-garbage-1-NUMBER" 1)
                                                (list "greeting-garbage-2-NUMBER" 2)))
(check-expect (flatten-json-pair tpair-2) (list (list "greeting-formal-STRING" "hello")
                                                (list "greeting-informal-STRING" "howdy")
                                                (list "greeting-garbage-NUMBER" 1)))

; flatten-json : JSON -> [List-of StringPair]
;return a [List-of (list String JSON)], but the second of any
; element of the list will not be a JSONArray or a JSONObject, annd first element describes
; the original "location" of that JSON
(define (flatten-json ajson)
  (cond
    [(string? ajson) (list (list "STRING" ajson))]
    [(number? ajson) (list (list "NUMBER" ajson))]
    [(boolean? ajson) (list (list "BOOLEAN" ajson))]
    [(symbol? ajson) (list (list "NULL" ajson))]
    [(empty? ajson) (list (list "EMPTY" ajson))]
    [(json-array? ajson) (number-array ajson )]
    [(json-object? ajson) (flatten-object ajson)]))
(check-expect (flatten-json
               (list 'null '()
                     (make-object
                         (list (list 'greeting
                                     (make-object
                                         (list (list 'formal "hello")
                                               (list 'informal "howdy")
                                               (list 'garbage (list true 1)))))))))
              (list (list "0-NULL" 'null)
                    (list "1-EMPTY" '())
                    (list "2-greeting-formal-STRING" "hello")
                    (list "2-greeting-informal-STRING" "howdy")
                    (list "2-greeting-garbage-0-BOOLEAN" true)
                    (list "2-greeting-garbage-1-NUMBER" 1)))

;JSONArray -> [List-of StringPair]
;Gives each JSON in the JSONArray an index, starting at 0 and incrementing by 1
(define (number-array ja)
  (combine-indices ja (build-index-list ja)))
(check-expect (number-array tarray-1) (list (list "0-BOOLEAN" #true)
                                            (list "1-NUMBER" 1)))
(check-expect (number-array tarray-1)(list (list "0-BOOLEAN" #true) (list "1-NUMBER" 1)))

;JSONArray -> [List-of String]
;The List returned is the size of the JSONARRAY, where each element is it's index (as a String)
(define (build-index-list l)
  (build-list (length l)  number->string))
(check-expect (build-index-list tlojp-1) (list "0" "1" "2"))
(check-expect (build-index-list (list 'a 'b 'c 'd 'e 'f)) (list "0" "1" "2" "3" "4" "5"))

;JSONArray [List-of String] -> [List-of StringPair]
;combines each JSONArray with it's index, which is containted in the [List-of String]
;**The JSONArray and list of Strings must be the same size
(define (combine-indices ja in)
  (cond [(empty? ja) '()]
        [(cons? ja) (append (append-json-string (first in)
                                                (flatten-json (first ja)))
                            (combine-indices (rest ja) (rest in)))]))
(check-expect (combine-indices tarray-1 (list "0" "1" "2"))
              (list (list "0-BOOLEAN" #true) (list "1-NUMBER" 1)))
              
;JSONObject -> [List-of StringPair]
;Unwraps a JSONObject's contents and flattens them
(define (flatten-object ob)
  (flatten-content (object-content ob)))
(check-expect (flatten-object tobject-2) (list (list "formal-STRING" "hello")
                                               (list "informal-STRING" "howdy")
                                               (list "garbage-NUMBER" 1)))
(check-expect (flatten-object tobject-1) (list (list "formal-STRING" "hello")
                                               (list "informal-STRING" "howdy")
                                               (list "garbage-0-BOOLEAN" #true)
                                               (list "garbage-1-NUMBER" 1)
                                               (list "garbage-2-NUMBER" 2)))

;JSONArray -> [List-of StringPair]
;Flattens a JSONArray
(define (flatten-arr ja)
  (cond [(empty? ja) '()]
        [(cons? ja) (append (flatten-json (first ja))
                            (flatten-arr (rest ja)))]))
(check-expect (flatten-arr tarray-1) (list (list "BOOLEAN" #true) (list "NUMBER" 1)))

(check-expect (flatten-arr (list 1 2 'apple '() 'null)) (list (list "NUMBER" 1)
                                                              (list "NUMBER" 2)
                                                              (list "NULL" 'apple)
                                                              (list "EMPTY" '())
                                                              (list "NULL" 'null)))
;;with first -> returns normal on its own, but anything w/ an array will only
;;  flatten the first element
;;without first -> returns a list of list of lists, and flatten-json-pair won't work

;;[List-of JSONPair] -> [List-of StringPair]
;;Flattens a List of JSONPair's (aka. a JSONObject's contents)
(define (flatten-content lojp)
  (cond
    [(empty? lojp)'()]
    [(cons? lojp)
     (append (flatten-json-pair (first lojp))
             (flatten-content (rest lojp)))]))
(check-expect (flatten-content tlojp-2) (list (list "formal-STRING" "hello")
                                              (list "informal-STRING" "howdy")
                                              (list "garbage-NUMBER" 1)))
(check-expect  (flatten-content tlojp-1) (list  (list "formal-STRING" "hello")
                                                (list "informal-STRING" "howdy")
                                                (list "garbage-0-BOOLEAN" #true)
                                                (list "garbage-1-NUMBER" 1)
                                                (list "garbage-2-NUMBER" 2)))


;;[List-of StringPair] String -> [List-of StringPair]
;;add the string to the list's string (aka (first list))
(define (append-json-string str j)
  (local (
          ;helper: String -> String
          (define (helper jp)
            (list (string-append str "-" (first jp))
                  (second jp))))
    (map helper j)))
(check-expect (append-json-string "label" tlosp-1) (list (list "label-formal" "hello")
                                                         (list "label-informal" "howdy")
                                                         (list "label-garbage" (list #true 1 2))))
(check-expect (append-json-string "directory" tlosp-2) (list (list "directory-formal" "hello")
                                                             (list "directory-informal" "howdy")
                                                             (list "directory-garbage" 1))) 

;;------------ Test Variables ------------
(define tlosp-1 (list (list "formal" "hello")
                      (list "informal" "howdy")
                      (list "garbage" (list #true 1 2))))

(define tlosp-2 (list (list "formal" "hello")
                      (list "informal" "howdy")
                      (list "garbage" 1)))

(define tlojp-1 (list (list 'formal "hello")
                      (list 'informal "howdy")
                      (list 'garbage (list #true 1 2))))

(define tlojp-2 (list (list 'formal "hello")
                      (list 'informal "howdy")
                      (list 'garbage 1)))

(define tobject-1 (make-object tlojp-1))

(define tobject-2 (make-object tlojp-2))

(define tpair-1 (list 'greeting tobject-1))

(define tpair-2 (list 'greeting tobject-2))

(define tpair-3 (list 'test3 "hello"))

(define tarray-1 (list true 1))

