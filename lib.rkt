#lang racket 

(provide file->content)

(require syntax/parse/define
         (for-syntax syntax/location))
(define-syntax (file->content stx)
  (syntax-parse stx
    [(_ path)
      #`(let ()
          (dynamic-require (build-path #,(syntax-source-directory stx) path) 'content))
       ]))


