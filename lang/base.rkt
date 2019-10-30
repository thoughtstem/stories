#lang at-exp racket

(provide (except-out (struct-out place) place)
         place-width place-height

         (except-out (struct-out character) character) 
         (except-out (struct-out time) time) 
         (except-out (struct-out story) story) 
         (except-out (struct-out story-link) story-link) 
         (rename-out ;[make-posn posn]
                     [make-place place]
                     [make-character character]
                     [make-time time]
                     [make-story story]
                     [make-story-link story-link])
         (rename-out [story-name story-title])

         (struct-out posn)
         
         story-flatten-links
         times-overlap?
         places-nearby?  )

(require posn)
(require gregor)

(struct place      (name data posn posn2)
  #:transparent)
(struct character  (name data)
  #:transparent)
(struct time       (name data start end)
  #:transparent)
(struct story      (name data place time characters links)
  #:transparent)
(struct story-link (data next-story))

(define (make-place name 
                    (posn #f) 
                    (posn2 #f) 
                    #:position (posns (list #f #f))
                    #:data (data #f))
  (place name data 
         (or posn (first posns)) 
         (or posn2 (second posns))))

(define (place-width p)
  (- (posn-x (place-posn2 p))
     (posn-x (place-posn p))))

(define (place-height p)
  (- (posn-y (place-posn2 p))
     (posn-y (place-posn p))))

(define (make-character name #:data (data #f))
  (character name data))

(define (make-time name start end #:data (data #f))
  (time name data start end))

(define (make-story name 
                    #:place place 
                    #:time time 
                    #:characters characters 
                    #:data (data #f) 
                    #:links (links '()))
  (->* (string? #:place place? #:time time? #:characters (listof character?)) (#:data any/c) story?)
  (story name data place time characters links))

(define (make-story-link next-story #:data (data #f))
  (story-link data next-story))



;Relationships

(define (in-time? lte gte t n)
  (and
    (gte (time-start t) n)
    (lte (time-end   t) n)))

(define (times-overlap? t1 t2
                        #:lte (lte <=) 
                        #:gte (gte >=)) 

  (define s1 (time-start t1))  
  (define s2 (time-start t2))  
  (define e1 (time-end t1))  
  (define e2 (time-end t2))  

  (or
    (in-time? lte gte t1 s2)
    (in-time? lte gte t1 e2)
    (in-time? lte gte t2 s1)
    (in-time? lte gte t2 s1)))


(define (distance p1 p2)
  (define p1-p (place-posn p1))
  (define p2-p (place-posn p2))

  (define x-dist (abs (- (posn-x p1-p) (posn-x p2-p))))
  (define y-dist (abs (- (posn-y p1-p) (posn-y p2-p))))

  (sqrt
    (+
      (sqr x-dist)
      (sqr y-dist))))

(define (places-nearby? p1 p2 #:radius (radius 20))
  (< (distance p1 p2) radius))



(define (story-flatten-links s)
  (if (empty? (story-links s)) 
    (list s)  
    (cons s (map story-flatten-links (story-links s)))))






