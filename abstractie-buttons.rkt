#lang racket


(require "turingobjectoriented.rkt")
(require "abstractie-button.rkt")

;Knop -> actie

;1 -> left
;2 -> step
;3 -> right
;1&2 -> pauze
;1&3 -> run
;2&3 -> write

(provide (all-defined-out))


(define (mk-knoppen PIN1 PIN2 PIN3)
  (let ((button1 (mk-button PIN1))
        (button2 (mk-button PIN2))
        (button3 (mk-button PIN3))
        (tm (turing "Simple incrementer"
                    'q0
                    'qf
                    'B
                    '(#(q0 1 1 right q0)
                      #(q0 B 1 stay  qf))))
        (finished? #f))


    (define (check)
      (cond ((and (button1 'pushed?)
                  (button2 'pushed?))
             (displayln "pauze")
             'pauze)
            ((and (button1 'pushed?)
                  (button3 'pushed?))
             (displayln "run")
             'run) ;hoe laat je die loop gebeuren en eindigen (in deze file of in abstractie-buttons?)
            ((and (button3 'pushed?)
                  (button2 'pushed?))
             (displayln "write!")
             'write!)
            ((button1 'pushed?)
             (displayln "left")
             'left)
            ((button2 'pushed?)
             (displayln "step")
             'step)
            ((button3 'pushed?)
             (displayln "right")
             'right)
            (else #f)))
            

    (define (step)
      (cond ((tm 'finished?)
             (displayln "Turing Machine is finished")
             (set! finished? #t))
          (else (displayln 'step)(tm 'step))))

    (define (write/left/right/step button)
      (if (ormap (lambda (x) (eq? button x)) (list 'write! 'left 'right))
          (tm button)
          (if (eq? 'step button)
              (step)
              (displayln "no button pushed"))))
      

    (let init ()
      (tm 'reset '(1 1 1)))
    
      
    (define (dispatch msg)
      (case msg
        ('step (step))
        ('check (check))
        ('finished? finished?)
        ('write/left/right/step write/left/right/step)))

    dispatch))

  