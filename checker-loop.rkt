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

    (when (eq? pushed-button 'run)
      (set! pauze? #f))

    (case pushed-button
      ((left right step write!)
               ((knoppen-lijst 'write/left/right/step) pushed-button)))  

    (gpio-delay-ms 500)

    (input-loop pauze?))
  
  (define (input-loop pauze?)
    (let ((pushed-button? (knoppen-lijst 'check))) ;de pauze check moet voor pushed-button komen
      (unless (knoppen-lijst 'finished?)
        (if pauze?
            (when pushed-button?
              (loop-pauze #t pushed-button?))
            (if (eq? pushed-button? 'pauze)
                (loop-pauze #t pushed-button?)
                (begin 
                  (knoppen-lijst 'step)
                  (gpio-delay-ms 2000))))
        (gpio-delay-ms 100)
        (input-loop pauze?))))  ;zet pauze-check op deze lijn (hoe zou je de run laten werken?)

  (input-loop #t))

(loops)
          
    

