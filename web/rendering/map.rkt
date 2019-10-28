#lang racket

(provide place-map)

(require (except-in website/bootstrap time frame))
(require stories/base
         2htdp/image
         (prefix-in h: lang/posn)
         )

(define (place-map places highlighted)
  (define w (places-width  places))
  (define h (places-height places))
  (define bg 
    (rectangle w h 
               'solid 'gray) )

  (write-img
    (place-images 
      (map place->icon places) 
      (map place->image-posn places) 
      bg)

    #;
    (scale-to-width 300
                    (place-images 
                      (map place->icon places) 
                      (map place->image-posn places) 
                      bg))))

(define (scale-to-width w i)
  (define current-w (image-width i))

  (scale (/ w current-w) i))

(define (places-width places)
  (define xs
    (map (compose posn-x place-posn) places))   
  (define max-x (apply max xs))
  (define min-x (apply min xs))

  (- max-x min-x))

(define (places-height places)
  (define ys
    (map (compose posn-y place-posn) places))   
  (define max-y (apply max ys))
  (define min-y (apply min ys))

  (- max-y min-y))

(define (place->icon p)
  (square 1 'solid 'red))

(define (place->image-posn place)
  (define p (place-posn place))
  (h:make-posn (posn-x p)
               (posn-y p)))
