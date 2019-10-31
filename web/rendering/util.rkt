#lang racket

(provide standard-renderer
         all-places
         all-characters
         all-times
         all-stories
         listing)

(require (except-in website/bootstrap time)
         gregor)
(require stories/base 
         "../paths.rkt"
         "./map.rkt"
         "./timeline.rkt")

(define all-places     (make-parameter '()))
(define all-characters (make-parameter '()))
(define all-times      (make-parameter '()))
(define all-stories    (make-parameter '()))

(define (listing header link-f things)
  (when (not (empty? things))
    (list
      (if (string? header)
        (h2 header)
        header)
      (ul
        (map (compose li link-f) things)))))

(define (card-listing header link things)
  (when (not (empty? things))
    (card
      (card-body
        (listing (card-title header) link
                 things)))))

(define (cols #:kind (kind col) . elements)
  (map kind 
       (filter-not void? elements)))

(define (index)
  (list
    (h1 "Index") 

    (listing "Places"     link-to-place     (all-places))
    (listing "Characters" link-to-character (all-characters))
    (listing "Times"      link-to-time      (all-times))
    (listing "Stories"    link-to-story     (all-stories))))

(define (render-place place)
  (define stories-here 
    (filter-stories-by-place (all-stories) place))

  (list 
    (h1 (place-name place))
    ;(place-map (all-places) place)
    (container
      (row
        (cols
          (card-listing "Nearby Places" link-to-place
                        (filter (curry places-nearby? place)
                                (remove place (all-places)))) 
          (card-listing "Characters here" link-to-character
                        (flatten (map story-characters stories-here))) 

          (card-listing "Times of stories here" link-to-time
                        (map story-time stories-here)) 

          (card-listing "Stories here" link-to-story stories-here)) ))
    
    (data-wrap (place-data place))
    )) 

(define (render-time time)
  (define stories-now
    (filter-stories-by-time (all-stories) time))

  (list 
    (h1 (time-name time))
    (container
      (row
        (cols
          (card-listing "Places with stories at this time" link-to-place
                        (map story-place stories-now))  
          (card-listing "Characters with stories at this time" link-to-character
                        (flatten (map story-characters stories-now)))  

          (card-listing "Overlapping Times" link-to-time
                        (filter 
                          (curry times-overlap? 
                                 #:lte moment<=?
                                 #:gte moment>=? time) 
                          (remove time (all-times))))

          (card-listing "Stories during this time" link-to-story
                        stories-now))))

    (data-wrap (time-data time))
    ))

(define (render-character character)
  (define stories-about (filter-stories-by-character (all-stories) character))
  (list 
    (h1 (character-name character))
    
    (row
      (cols
        (card-listing "Places character appears" link-to-place
                      (map story-place stories-about))  
        (card-listing "Related characters" link-to-character
                      (remove character
                              (remove-duplicates (flatten (map story-characters stories-about)))))  
        (card-listing "Times character appears" link-to-time
                      (map story-time stories-about))  
        (card-listing "Stories about this character" link-to-story
                      stories-about)))  
    

    (data-wrap (character-data character))
    ))

(define (render-story story)
  (list 
    (container
      class: "story-meta-data" 
      (h1 (story-name story))
      (row
        (col-4
          (card
            (card-body
              (card-title "Place")
              (card-link
                (link-to-place (story-place story))))))  
        (col-4
          (card
            (card-body
              (card-title "Time")
              (card-link
                (link-to-time (story-time story))))))  
        (col-4
          (card
            (card-body
              (listing (card-title "Characters") 
                       link-to-character
                       (story-characters story)))))))
    (data-wrap (story-data story))
    (listing "Linked Stories" link-to-story
             (story-links story))))

(define (data-wrap d)
  (container d))

(define (standard-renderer)
  (lambda (thing) 
    (content
      (navbar #:brand "home")
      (container
        (cond
          [(eq? thing 'index) (index)]
          [(place? thing) (render-place thing)]
          [(character? thing) (render-character thing)]
          [(story? thing) (render-story thing)]
          [(time? thing) (render-time thing)])))))

