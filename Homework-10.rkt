;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hw10) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct page [title content])
 
; A Wiki is a [List-of Page]
; A Page is a (make-page Symbol [List-of Symbol])
; INTERPRETATION: An individual page contains a title and a
; list of links to other pages (represented by their titles).

;out-links: Symbol Wiki -> [List-of Symbol]
;returns all pages (in form of their titles) in the wiki that
;the page w/ the given symbol as the title links to
(define (out-links s w)
  (cond [(empty? w) '()]
        [else (if (symbol=? s (page-title (first w)))
                  (page-content (first w))
                  (out-links s (rest w)))]))
(check-expect (out-links 'apple (list (make-page 'fruit '(apple bannana pineapple))
                                      (make-page 'red-things '(apple fire-truck cherry))
                                      (make-page 'apple '(red sweet vegan))
                                      (make-page 'vegan '(veggies fruit beans))))
              '(red sweet vegan))
(check-expect (out-links 'carrot (list (make-page 'fruit '(apple bannana pineapple))
                                       (make-page 'red-things '(apple fire-truck cherry))
                                       (make-page 'apple '(red sweet vegan))
                                       (make-page 'vegan '(veggies fruit beans))))
              '())
(check-expect (out-links 'apple '()) '())

;in-links: Symbol Wiki -> [List-of Symbol]
;returns all pages (in form of their titles) that link to the given page.
(define (in-links s w)
  (cond [(empty? w) '()]
        [else (if (ormap (λ(s1) (symbol=? s1 s))
                         (page-content (first w)))
                  (cons (page-title (first w)) (in-links s (rest w)))
                  (in-links s (rest w)))]))
(check-expect (in-links 'apple (list (make-page 'fruit '(apple bannana cherry pineapple))
                                     (make-page 'red-things '(apple fire-truck cherry))
                                     (make-page 'apple '(red sweet vegan))
                                     (make-page 'vegan '(veggies fruit beans))))
              '(fruit red-things))
(check-expect (in-links 'carrot (list (make-page 'fruit '(apple bannana pineapple))
                                      (make-page 'red-things '(apple fire-truck cherry))
                                      (make-page 'apple '(red sweet vegan))
                                      (make-page 'vegan '(veggies fruit beans))))
              '())
(check-expect (in-links 'fire-truck '()) '())

;cycle: Wiki -> Boolean
;determins if there is a way to start on some page in the Wiki, follow links,
;and end up back at the same page?
;Note: the actual generative recursion occurs in the cycle-in-page? function
(define (cycle? lop)
  (ormap (λ(pg) (cycle-in-page? pg lop)) lop))
(check-expect (cycle? (list (make-page 'fruit '(apple bannana cherry pineapple)) ; [S]fruit -> apple
                            (make-page 'red-things '(apple fire-truck cherry)) 
                            (make-page 'apple '(red sweet vegan)) ;apple -> vegan
                            (make-page 'vegan '(veggies fruit beans)))) ;vegan -> [E]fruit
              #true)
(check-expect (cycle? (list (make-page 'apple '(red sweet vegan)) ;[S]apple -> vegan
                            (make-page 'fruit '(apple bannana cherry pineapple)) ;fruit -> [E]apple
                            (make-page 'red-things '(apple fire-truck cherry)) 
                            (make-page 'vegan '(veggies fruit beans)))) ;vegan -> fruit
              #true)
(check-expect (cycle? (list (make-page 'apple '(red sweet vegetarian))
                            (make-page 'fruit '(apple bannana cherry pineapple))
                            (make-page 'red-things '(apple fire-truck cherry)) 
                            (make-page 'vegan '(veggies fruit beans))))
              #false)
(check-expect (cycle? (list (make-page 'fruit '(bannana cherry pineapple))
                            (make-page 'fire-truck '(8-wheels 4-people 12-inches Queen-Liz ships))
                            (make-page 'red-things '(apple fire-truck cherry));[S]red-things -> apple
                            (make-page 'rulers '(Queen-Liz Vlad Lenin Aoun))
                            (make-page 'Lenin '(Vlad Russia radical))
                            (make-page 'vegan '(veggies fruit beans));vegan -> beans
                            (make-page 'beans '(pinto peas refried cheese jumping)) ;beans -> peas
                            (make-page 'apple '(healthy sweet vegan));apple -> vegan
                            (make-page 'Aoun '(prez robot-proof co-op))
                            (make-page 'black-eye '(band injury black));black-eye -> black
                            (make-page 'communism '(Marx Cold-War Lenin))
                            (make-page 'red '(hues communism red-things)) ;red -> red-things[E]
                            (make-page 'black '(color light));black -> color
                            (make-page 'color '(yellow blue red purple)) ;color -> red
                            (make-page 'peas '(chick green black-eye)))) ;peas -> black-eye
              #true)
(check-expect (cycle? (list (make-page 'fruit '(bannana cherry pineapple))
                            (make-page 'red-things '(apple fire-truck cherry))
                            (make-page 'communism '(Marx Cold-War Lenin))
                            (make-page 'apple '(healthy sweet vegan))
                            (make-page 'vegan '(veggies fruit beans))
                            (make-page 'beans '(pinto black-eye refried cheese jumping))
                            (make-page 'black-eye '(band injury black))
                            (make-page 'black '(color light))
                            (make-page 'color '(yellow blue green purple))
                            (make-page 'red '(hues communism red-things))
                            (make-page 'Lenin '(Vlad Russia radical))))
              #false)
(check-expect (cycle? (list (make-page 'red-things '(apple fire-truck cherry))
                            (make-page 'apple '(red sweet vegan))
                            (make-page 'vegan '(veggies fruit beans))))
              #false)
(check-expect (cycle? '()) #false)

;cycle-in-page: Page Wiki -> Boolean
;finds if there is a cycle in the wiki that originate from the page
;
;how: Is a link of the Page itself in it's contents?
;     YES: there is a cycle
;     NO: take the page, and unwrap any contents that the page's contents have,then test again
;
;termination argument:  when the unwrapped page is empty
;
(define (cycle-in-page? pg awiki)
  (local [(define unwrapped-content (unwrap-symbols (page-content pg) awiki))]
    (cond [(empty? unwrapped-content) #false]
          [else (if (contains-self? pg)
                    #true
                    (cycle-in-page?
                     (make-page (page-title pg) unwrapped-content)
                     awiki))])))
(check-expect (cycle-in-page? (make-page 'fruit '(apple bananna pineapple))
                              (list (make-page 'fruit '(apple bananna pineapple))
                                    (make-page 'apple '(red sweet healthy))
                                    (make-page 'healthy '(veggies fruit beans))))
              #true)
(check-expect (cycle-in-page? (make-page 'fruit '(apple bananna pineapple))
                              (list (make-page 'fruit '(apple bananna pineapple))
                                    (make-page 'apple '(red sweet healthy))
                                    (make-page 'healthy '(veggies cheese beans))))
              #false)
(check-expect (cycle-in-page? (make-page 'Aoun '(prez robot-proof co-op)) '()) #f)

;unwrap-symbols: [List-of Symbol] Wiki (should be rest)-> [List-of Symbol]
;converts each symbol that is in the wiki into it's contents, and discards all other symbols
(define (unwrap-symbols los awiki)
  (cond [(empty? los) '()]
        [else (local [(define maybe-contents (out-links (first los) awiki))]
                (if (empty? maybe-contents)
                    (unwrap-symbols (rest los) awiki)
                    (append maybe-contents (unwrap-symbols (rest los) awiki))))]))
(check-expect (unwrap-symbols '(apple bananna pineapple)
                              (list (make-page 'apple '(red sweet healthy))
                                    (make-page 'healthy '(fruit veggies 'beans))))
              (list 'red 'sweet 'healthy))
(check-expect (unwrap-symbols '(red sweet healthy bananna pineapple)
                              (list (make-page 'apple '(red sweet healthy))
                                    (make-page 'healthy '(fruit veggies beans))))
              (list 'fruit 'veggies 'beans ))
(check-expect (unwrap-symbols '(a b c) '()) '())
(check-expect (unwrap-symbols '() (list (make-page 'apple '(red sweet healthy))
                                        (make-page 'healthy '(fruit veggies 'beans)))) '())

;contains-self? Page -> Boolean
;Determines if the page has a link to itself in its contents
(define (contains-self? mpage)
  (ormap (λ(t1) (symbol=? t1 (page-title mpage))) (page-content mpage)))
(check-expect (contains-self? (make-page 'words '(aardvark house words))) #t)
(check-expect (contains-self? (make-page 'red-things '(apple fire-truck cherry))) #f)
(check-expect (contains-self? (make-page 'fire-truck '())) #f)













