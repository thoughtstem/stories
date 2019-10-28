#lang at-exp racket

(provide (except-out (struct-out place) place)
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
         
         times-overlap?
         places-nearby?  )

(require posn)

(struct place      (name data posn posn2))
(struct character  (name data))
(struct time       (name data start end))
(struct story      (name data place time characters links))
(struct story-link (data next-story))

(define (make-place name posn posn2 #:data (data #f))
  (place name data posn posn2))

(define (make-character name #:data (data #f))
  (character name data))

(define (make-time name start end #:data (data #f))
  (time name data start end))

(define (make-story name 
                    #:place place 
                    #:time time 
                    #:characters characters 
                    #:data (data #f) #:links (links '()))
  (story name data place time characters links))

(define (make-story-link next-story #:data (data #f))
  (story-link data next-story))



;Relationships

(define (in-time? t n)
  (and
    (<= (time-start t) n)
    (>= (time-end t)   n)))

(define (times-overlap? t1 t2)
  (define s1 (time-start t1))  
  (define s2 (time-start t2))  
  (define e1 (time-end t1))  
  (define e2 (time-end t2))  

  (or
    (in-time? t1 s2)
    (in-time? t1 e2)
    (in-time? t2 s1)
    (in-time? t2 s2)))



(define (distance p1 p2)
  (define p1-p (place-posn p1))
  (define p2-p (place-posn p2))

  (define x-dist (abs (- (posn-x p1-p) (posn-x p2-p))))
  (define y-dist (abs (- (posn-y p1-p) (posn-y p2-p))))

  (sqrt
    (+
      (sqr x-dist)
      (sqr y-dist))))

(define (places-nearby? p1 p2 #:radius (radius 10))
  (< (distance p1 p2) radius))

