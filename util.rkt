#lang racket

(provide define-list
         define/provide-list
         defines-from-directory)

(require syntax/parse/define
         (for-syntax racket
                     file/glob))

(define-syntax-rule (define-list name (def id thing) ...)
  (begin
    (define id thing) ...
    (define name (list id ...))))

(define-syntax-rule (define/provide-list name (def id thing) ...)
  (begin
    (provide id) ...
    (provide name)
    (define id thing) ...
    (define name (list id ...))))

(define-for-syntax (containing-directory path)
  (apply build-path (drop-right (explode-path path) 1)))

(define-for-syntax (file-name-only path)
  (string->symbol
    (~a
      (path-replace-extension
        (last (explode-path path))
        ""))))

(define-syntax (defines-from-directory stx)
  (syntax-parse stx
    [(_ dir #:wrap-each wrap #:all-as-list list-id)
     (define files 
       (glob (build-path 
               (containing-directory (syntax-source stx))
               (syntax->datum #'dir)
               "*.rkt")))
     (datum->syntax stx
                    (syntax->datum
                      #`(define/provide-list list-id
                                             #,@(map 
                                                  (lambda (f)
                                                    (define n (file-name-only f)) 
                                                    #`(define #,n
                                                        (wrap #,f)))
                                                  files))))]))


