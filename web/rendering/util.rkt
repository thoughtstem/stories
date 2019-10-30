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
      (h2 header)
      (map (compose div link-f) things))))

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
    (data-wrap (place-data place))
    (listing "Nearby Places" link-to-place
             (filter (curry places-nearby? place)
               (remove place (all-places))))
    (listing "Characters here" link-to-character
             (flatten (map story-characters stories-here)))
    (listing "Times of stories here" link-to-time
             (map story-time stories-here))
    (listing "Stories here" link-to-story stories-here))) 

(define (render-time time)
  (define stories-now
    (filter-stories-by-time (all-stories) time))

  (list 
    (h1 (time-name time))
    ;(timeline (all-times) time)
    (data-wrap (time-data time))
    (listing "Places with stories at this time" link-to-place
             (map story-place stories-now))  
    (listing "Characters with stories at this time" link-to-character
             (flatten (map story-characters stories-now)))  

    (listing "Overlapping Times" link-to-time
             (filter 
               (curry times-overlap? 
                      #:lte moment<=?
                      #:gte moment>=? time) 
               (remove time (all-times))))

    (listing "Stories during this time" link-to-story
             stories-now)))

(define (render-character character)
  (define stories-about (filter-stories-by-character (all-stories) character))
  (list 
    (h1 (character-name character))
    
    (data-wrap (character-data character))
    (listing "Places character appears" link-to-place
             (map story-place stories-about))  
    (listing "Related characters" link-to-character
             (remove character
                     (remove-duplicates (flatten (map story-characters stories-about)))))  
    (listing "Times character appears" link-to-time
             (map story-time stories-about))  
    (listing "Stories about this character" link-to-story
             stories-about)  ))

(define (render-story story)
  (list 
    (h1 (story-name story))
    (h2 "Place")
    (link-to-place (story-place story))  
    (h2 "Time")
    (link-to-time (story-time story))  
    (listing "Characters" link-to-character
             (story-characters story))
    (data-wrap (story-data story))))

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

