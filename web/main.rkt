#lang racket

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

  (parameterize ([all-places places]
                 [all-characters characters]
                 [all-times times]
                 [all-stories stories])

    (define index-page (page index.html
                             (renderer 'index)))

    (flatten
      (list
        (bootstrap-files)
        index-page
        (places->pages renderer places)
        (characters->pages renderer characters)
        (times->pages renderer times)
        (stories->pages renderer stories)))))

(module+ test
  (define neighborhood     
    (place "Neighborhood"     
           (posn 0 0)
           (posn 50 50)))

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






