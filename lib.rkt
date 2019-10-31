#lang racket 

(provide 
  file->content
  place-from
  character-from
  time-from
  story-from)

(require syntax/parse/define
         stories/base
         (for-syntax syntax/location))
(define-syntax (file->content stx)
  (syntax-parse stx
    [(_ path)
      #`(let ()
          (dynamic-require (build-path #,(syntax-source-directory stx) path) 'content))
       ]))


(require syntax/parse/define
         (for-syntax syntax/location))

(define-syntax (story-from stx)
  (syntax-parse stx
    [(_ path)
      #`(let ()
          (path->story (build-path #,(syntax-source-directory stx) path)))]))

(define (path->story path)
  (define title          (dynamic-require path 'title)) 
  (define place          (dynamic-require path 'place)) 
  (define characters     (dynamic-require path 'characters)) 
  (define time           (dynamic-require path 'time)) 
  (define content        (dynamic-require path 'content)) 
  (define story-links    (dynamic-require path 'links)) 

  (story title
         #:place place
         #:time time
         #:characters characters
         #:links story-links
         #:data content))


(define-syntax (character-from stx)
  (syntax-parse stx
    [(_ path)
      #`(let ()
          (path->character (build-path #,(syntax-source-directory stx) path)))]))

(define (path->character path)
  (define name          (dynamic-require path 'name)) 
  (define content     (dynamic-require path 'content)) 
  (character name
             #:data content))

(define-syntax (place-from stx)
  (syntax-parse stx
    [(_ path)
      #`(let ()
          (path->place (build-path #,(syntax-source-directory stx) path)))]))

(define (path->place path)
  (define name     (dynamic-require path 'name)) 
  (define position     (dynamic-require path 'position)) 
  (define content     (dynamic-require path 'content)) 
  (place name
         #:position position
         #:data content))

(define-syntax (time-from stx)
  (syntax-parse stx
    [(_ path)
      #`(let ()
          (path->time (build-path #,(syntax-source-directory stx) path)))]))

(define (path->time path)
  (define name     (dynamic-require path 'name)) 
  (define start     (dynamic-require path 'start)) 
  (define end     (dynamic-require path 'end)) 
  (define content     (dynamic-require path 'content)) 
  (time name
        start
        end
        #:data content))
