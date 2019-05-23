#lang racket

(require "raspi-gpio.rkt")

(gpio-setup)
(gpio-mcp23017-setup 100 #x21)

(provide (all-defined-out))

(define (mk-led PIN)
  (let ((on? #f))
    
    (define (on)
      (set! on? #t)
      (gpio-digital-write PIN 1))

    (define (off)
      (set! on? #f)
      (gpio-digital-write PIN 0))

    (define (toggle)
      (if on? (off)
          (on)))

    (define (init)
      (gpio-set-pin-mode PIN 'output))

    (init)

    (define (dispatch-led msg)
      (case msg
        ('on (on))
        ('off (off))
        ('toggle (toggle))
        ('state (if on? 'on 'off))))
    
    dispatch-led))