#lang racket

(require "abstractie-buttons.rkt")

(require "raspi-gpio.rkt")

(gpio-setup)

(define PIN1 23)
(define PIN2 24)
(define PIN3 25)

(define knoppen-lijst (mk-knoppen PIN1 PIN2 PIN3))


(define (loops)
  (define (loop-pauze pauze? pushed-button)
    
    (when (eq? pushed-button 'pauze) 
      (set! pauze? #t))

    (if (eq? pushed-button 'run)
               (begin (knoppen-lijst 'step)
                      (set! pauze? #f)) 
               ((knoppen-lijst 'write/left/right/step) pushed-button))  

    (gpio-delay-ms 1000)

    (input-loop pauze?))
  
  (define (input-loop pauze?)
    (let ((pushed-button? (knoppen-lijst 'check)))
      (when pushed-button?
          (if pauze?
              (loop-pauze #f pushed-button?)
              (unless (knoppen-lijst 'finished?) 
                ((gpio-delay-ms 1000)
                 (knoppen-lijst 'step)
                 (input-loop pauze?)))))

      (gpio-delay-ms
      (input-loop pauze?))))

  (input-loop #f))
          
    

