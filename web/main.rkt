#lang racket

(provide stories->site
         (all-from-out "./paths.rkt"))

(require (except-in website/bootstrap time))
(require stories/base 
         "./paths.rkt"
         "./rendering.rkt")

;The main function here is stories->site
;Assume that the data stored in each is some web content
(define (stories->site #:places (places '()) 
                       #:characters (characters '())
                       #:times (times '())
                       #:renderer (renderer (standard-renderer))
                       . stories)

  (parameterize ([all-places (flatten places)]
                 [all-characters (flatten characters)]
                 [all-times (flatten times)]
                 [all-stories (flatten stories)])

    (define index-page (page index.html
                             (renderer 'index)))

    (flatten
      (list
        (bootstrap-files)
        index-page
        (places->pages renderer (flatten places))
        (characters->pages renderer (flatten characters))
        (times->pages renderer (flatten times))
        (stories->pages renderer (flatten stories))))))

(module+ test
  (define neighborhood     
    (place "Neighborhood"     
           (posn 0 0)
           (posn 100 100)))

  (define bobs-house   (place "Bob's House" 
                              (posn 25 25)
                              (posn 26 26)
                              #:data (h1 "BOB")))
  (define alices-house   (place "Alice's House" 
                                (posn 25 30)
                                (posn 27 32)
                              #:data (h1 "ALICE")))

  (define bob          (character "Bob"))
  (define alice        (character "Alice"))

  (define one-morning  (time "One Morning" 0 2))

  (define bobs-story   (story "Bob's Morning Story" 
                              #:place bobs-house 
                              #:time one-morning
                              #:characters (list alice bob)))
  
  (render
    (stories->site 
      #:places     (list neighborhood bobs-house alices-house)
      #:characters (list alice bob)
      #:times      (list one-morning)
      bobs-story)
    #:to "out")
  
  )






