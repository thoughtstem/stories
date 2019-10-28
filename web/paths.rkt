#lang racket

(provide
  places->pages
  characters->pages
  times->pages
  stories->pages
  place-path
  character-path
  time-path
  story-path
  link-to-place
  link-to-character
  link-to-time
  link-to-story
  make-place-page
  make-character-page
  make-time-page
  make-story-page
  urlify)

(require stories/base)
(require (except-in website/bootstrap time))

(define (urlify s)
  (string-downcase
    (string-replace #:all? #t s #rx" " "-")))

(define (place-path p)
  (list "places" 
        (~a (urlify (place-name p)) 
            ".html")))

(define (character-path p)
  (list "characters"
        (~a (urlify (character-name p)) 
            ".html")))

(define (time-path p)
  (list "times"
        (~a (urlify (time-name p)) 
            ".html")))

(define (story-path p)
  (list "stories"
        (~a (urlify (story-name p)) 
            ".html")))

(define (link-to-place p)
  (link-to
    (place-path p)
    (place-name p)))

(define (link-to-character p)
  (link-to
    (character-path p)
    (character-name p)))

(define (link-to-time p)
  (link-to
    (time-path p)
    (time-name p)))

(define (link-to-story p)
  (link-to
    (story-path p)
    (story-name p)))


(define (make-place-page place place-content)
  (define path (place-path place))
  (page path place-content))

(define (make-character-page character character-content)
  (define path (character-path character))
  (page path character-content))

(define (make-time-page time time-content)
  (define path (time-path time))
  (page path time-content))

(define (make-story-page story story-content)
  (define path (story-path story))
  (page path story-content))


(define (places->pages renderer places)
  (map make-place-page
       places
       (map renderer
            places)))

(define (characters->pages renderer characters)
  (map make-character-page
       characters
       (map renderer
            characters)))

(define (times->pages renderer times)
  (map make-time-page
       times
       (map renderer
            times)))


(define (stories->pages renderer stories)
  (map make-story-page
       stories
       (map renderer
            stories)))


