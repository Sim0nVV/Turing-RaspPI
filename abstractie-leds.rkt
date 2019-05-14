#lang racket

(require "abstractie-led.rkt")

(provide mk-leds)

(define (mk-leds list-PINs)
  (let ((LEDs-list (map (lambda (x) (mk-led x))
                        list-PINs)))
    
    (define (fill-list lijst)
      (append lijst (build-list (- 10
                                  (length lijst))
                               (lambda (x) 'off))))
 

    (define (change-leds! on/off-list)
      (define (change-leds on/off-list)
        (for-each (lambda (led on/off) (led on/off)) LEDs-list on/off-list))
                  
      (if (= (length on/off-list) 10)
        (change-leds on/off-list)
        (change-leds (fill-list on/off-list))))
        

      

    (define (dispatch msg)
      (case msg
        ('change-leds! change-leds!)))

    dispatch))