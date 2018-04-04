#lang racket
; ----- PROVIDES -----
(provide string->json
         get-url
         json-array?
         json-object?
         json-pair?
         make-object
         object-content
         object?
         TODAY-FORECAST-URL
         DAILY-FORECAST-URL)

; ----- REQUIRES -----
(module+ test
  (require rackunit))
(require json)
(require net/url)

; ----- DATA DEFINITIONS -----
; A JSON is one of:
; - String
; - Number
; - Boolean
; - 'null
; - JSONArray
; - JSONObject

;;A JSONArray is a [List-of JSON])

;;A JSONObject is a (make-object [List-of JSONPair])
(define-struct object [content] #:transparent)

;;A JSONPair is a (list Symbol JSON)

;;A PartialJson is one of:
; - String
; - Number
; - Boolean
; - 'null
; - [List-of PartialJson]
; - [HashMap Symbol PartialJson]

#|--- CONSTANTS ---|#
(define TODAY-FORECAST-URL
  "http://api.openweathermap.org/data/2.5/weather?q=Boston&units=imperial&APPID=540bc3b21f960cf9c30f280ae5c2bf01")
(define DAILY-FORECAST-URL
  "http://api.openweathermap.org/data/2.5/forecast/daily?q=Boston&units=imperial&APPID=540bc3b21f960cf9c30f280ae5c2bf01")

; ----- JSON PARSING FUNCTIONS -----

;;parse-hashmaps : PartialJson -> Json
;;Parse the hashmaps into association lists
(define (parse-hashmaps pj)
  (cond [(cons? pj) (map parse-hashmaps pj)]
        [(hash? pj) (make-object
                        (hash-map pj (λ (x y) (list x (parse-hashmaps y)))))]
        [else pj]))
(module+ test
  (check-equal? (parse-hashmaps "hi") "hi")
  (check-equal? (parse-hashmaps (list (make-immutable-hasheq (list (cons 'a 1)))
                                      (make-immutable-hasheq (list (cons 'b true)))
                                      (make-immutable-hasheq (list (cons 'c "hello")))))
                (list (make-object '((a 1)))
                      (make-object '((b #t)))
                      (make-object '((c "hello"))))))

;; string->json : RawJsonString -> JSon
;; Parse the string
(define (string->json rjs)
  (parse-hashmaps (string->jsexpr rjs)))

;;json? : Any -> Boolean
;;Checks if this thing is a JSON
(define (json? x)
  (or (string? x) (number? x) (boolean? x)
      (and (symbol? x) (symbol=? x 'null))
      (json-array? x) (json-object? x)))
(module+ test
  (check-true (json? "hi"))
  (check-true (json? 3))
  (check-true (json? #false))
  (check-true (json? 'null))
  (check-true (json? '("hi" 3 #false null)))
  (check-true (json? (make-object '()))))

;;json-array? : Any -> Boolean
;;Checks if this thing is a JSONArray
(define (json-array? x)
  (and (or (empty? x) (cons? x))
       (andmap json? x)))
(module+ test
  (check-true (json-array? '()))
  (check-true (json-array? '("hi" 3 #false null)))
  (check-false (json-array? '("hi" bye))))

;;json-object? : Any -> Boolean
;;Checks if this thing is a JSONObject
(define (json-object? x)
  (and (object? x)
       (or (empty? (object-content x))
           (cons? (object-content x)))
       (andmap json-pair? (object-content x))))
(module+ test
  (check-false (json-object? '()))
  (check-true (json-object? (make-object '())))
  (check-true (json-object? (make-object '((a 12) (b null))))))

;;json-pair? : Any -> Boolean
;;Checks if this thing is a JSONPair
(define (json-pair? x)
  (and (cons? x) (= (length x) 2)
       (symbol? (first x)) (json? (rest x))))

; ----- HTTP FUNCTIONS -----
; String -> String
(define (get-url str)
  (string-trim (bytes->string/utf-8
    (port->bytes (get-pure-port (string->url str))))))

