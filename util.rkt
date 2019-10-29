#lang racket

(provide define-list
         define/provide-list)

(require syntax/parse/define)

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
