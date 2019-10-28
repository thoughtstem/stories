#lang racket

(provide render-city
         filter-stories-by-place 
         filter-stories-by-time
         filter-stories-by-character)

(require "./base.rkt" 2htdp/image)

(define (filter-stories-by-thing ss story-thing thing)
  (filter 
    (lambda (s)
      (eq? thing (story-thing s)))
    ss))

(define (filter-stories-by-place ss p)
  (filter-stories-by-thing ss story-place p))

(define (filter-stories-by-time ss t)
  (filter-stories-by-thing ss story-time t))

(define (filter-stories-by-character ss c)
  (filter 
    (lambda (s)
      (member c (story-characters s)))
    ss))

(define (render-city places
                     #:highlight (highlight #f)
                     (w 76) 
                     (h 48))

  (define bg (rectangle w h 'solid 'transparent))  

  (scale 5
         (place-images
             (map (curry place-icon
                         #:highlight highlight) 
                  places) 
             (map place-posn places) 
             bg)))

(define (place-icon p
                    #:highlight (highlight #f) )
  (if (and highlight
           (equal? p highlight))
    (star 3 'solid 'yellow)
    (square 1 'solid 'green))) 

