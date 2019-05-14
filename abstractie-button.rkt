#lang racket

(require "raspi-gpio.rkt")

(gpio-setup)

(provide mk-button)

(define (mk-button PIN)
    
  (define (state)
    (if (= 0 (gpio-digital-read PIN))
        'pushed
        'released))

  (define (pushed?)
    (eq? (state) 'pushed))
    
   (define (init)
     (gpio-set-pin-mode PIN 'input)
     (gpio-set-pull-resistor PIN 'up))

  (init)


   (define (dispatch msg)
     (case msg
       ('pushed? (pushed?))
       ('state (state))))

   dispatch)