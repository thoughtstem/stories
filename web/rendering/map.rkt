#lang racket

(provide place-map)

(require (except-in website/bootstrap time frame))
(require stories/base
         2htdp/image
         (prefix-in h: lang/posn))

(define (place-map places highlighted)
  (define w (places-width  places))
  (define h (places-height places))

  (define bg 
    (rectangle (add1 w) (add1 h) 
               'solid 'transparent) )

  (list
    (write-img
      style: (properties float: "right")
      (scale-to-width 300
                      (place-images/align
                        (map (curry place->icon
                                    #:highlight highlighted) places) 
                        (map place->image-posn places) 
                        "left" "top"
                        bg)))))

(define (scale-to-width w i)
  (define current-w (image-width i))

  (scale (/ w current-w) i))

(define (places-width places)
  (define ws (map place-width places))   
  (apply max ws))

(define (places-height places)
  (define hs (map place-height places))   
  (apply max hs))

(define (place->icon p #:highlight highlight)
  (overlay/align "middle" "top"
    (scale-to-width (place-width p)
      (text (place-name p) 12 'black))
    (rectangle (place-width p)
               (place-height p) 
               'outline 

               (if (eq? p highlight)
                 'red
                 'black)
               
               )))

(define (place->image-posn place)
  (define p (place-posn place))
  (h:make-posn 
    (posn-x p) 
    (posn-y p)))



