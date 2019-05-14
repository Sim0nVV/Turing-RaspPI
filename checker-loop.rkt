#lang racket



(require "abstractie-buttons.rkt")

(require "raspi-gpio.rkt")

(gpio-setup)

(define PIN1 23)
(define PIN2 24)
(define PIN3 25)

(define knoppen-lijst (mk-knoppen PIN1 PIN2 PIN3))

(let loop
  ((pauze? #t)
   (pushed-button (knoppen-lijst 'check)))
  
  (when (eq? pushed-button 'pauze) 
    (set! pauze? #t))

  
  (if pauze?
      (if (eq? pushed-button 'run)
          (begin (knoppen-lijst 'step)
                 (loop #f))
          ((knoppen-lijst 'write/left/right/step) pushed-button))
        (knoppen-lijst 'step))
    

  (gpio-delay-ms 1000)

  (unless (knoppen-lijst 'finished?)
    (loop pauze? (knoppen-lijst 'check))))