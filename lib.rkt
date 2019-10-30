#lang racket 

(provide 
  file->content
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


