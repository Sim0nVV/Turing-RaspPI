#lang racket

(require "abstractie-leds.rkt")

(require racket/format)

(provide (all-defined-out))

(define list-pin-leds (map (lambda (x) (+ 100 x)) '(0 1 2 3 4 11 12 13 14 15)))

(define (turing name state-init state-term blank rules)
  (let ((leds (mk-leds list-pin-leds)))

    (define state  state-init)
    (define head   50000)
    (define bottom 50000)
    (define top    50000)
    (define steps   0)
    (define tape   (make-vector 100000 blank))

    (define (init)
      (set! state  state-init)
      (set! head   50000)
      (set! bottom 50000)
      (set! top    50000)
      (set! steps   0)
      (set! tape   (make-vector 100000 blank)))
  
    (define (show)
      (display " steps: ")  (display (~a steps  #:align 'right #:width 2  #:pad-string " "))
      (display " state: ") (display (~a state #:align 'left  #:width 5  #:pad-string " "))
      (display " tape: ")
      (do ((pos bottom (+ pos 1))) ((> pos top))
        (display (if (= pos head) "[" " "))
        (display (vector-ref tape pos))
        (display (if (= pos head) "]" " ")))
      (newline))


    (define (write!)
      (let ((head-symbol (vector-ref tape head)))
        (cond ((eq? head-symbol blank)
               (vector-set! tape head 1))
              ((eq? head-symbol 1)
               (vector-set! tape head blank)))))

    (define (set-head! symbol)
      (vector-set! tape head symbol))

    (define (move action) 
      (case action
        ((left)
         (set! head (- head 1))
         (cond
           ((< head bottom)
            (set! bottom head)
            (set-head! blank))))
        ((right)
         (set! head (+ head 1))
         (cond
           ((> head top)
            (set! top head)
            (set-head! blank))))))
  
    (define (step! output)
      (cond (output
             (show)))
    
      (let try
        ((rules rules))
      
        (unless
            (null? rules)
          (let* ((rule          (car rules))
                 (state-before  (vector-ref rule 0))
                 (symbol-before (vector-ref rule 1)))
            (if (and (eq? state-before state)
                     (eq? symbol-before (vector-ref tape head)))
                (let ((symbol-after (vector-ref rule 2))
                      (action       (vector-ref rule 3))
                      (state-after  (vector-ref rule 4)))
                  (set-head! symbol-after)
                  (move action)
                  (set! state state-after))
          
          
                (try (cdr rules))))))
      (set! steps (+ steps 1)))

    (define (run)
      
      (let step-loop ()
        (if (eq? state-term state) ;Wat als state op blank state staat?
            (show)
            (begin
              (step! #t)
              (step-loop))))
    
      (newline) (newline))

    (define (reset symbols)
      (init)
      (do ()((null? symbols))
        (vector-set! tape top (car symbols))
        (set! symbols (cdr symbols))
        (set! top (+ top 1)))
      (vector-set! tape top blank))

    (define (led-value symbol)
      (if (eq? symbol blank)
          'off
          'on))


    (define head-offset 4)
    
    (define (list-leds)
      (define lijst-symbolen '())
      (do ([pos (- head head-offset) (+ pos 1)])
        ((= (length lijst-symbolen) 10)
         (reverse lijst-symbolen))
        (set! lijst-symbolen (cons (led-value
                                    (vector-ref tape pos))
                                   lijst-symbolen))))      
    
    
  (define (dispatch msg . argument)
    
    (when (not (null? argument))
      (set! argument (car argument)))
    (case msg
      ('reset (reset argument)
              (show))
      ('show (show))
      ('write! (write!)
               (show))
      ('left (move 'left)
             (show))
      ('right (move 'right)
              (show))
      ('step 
       (step! #t)
       (show))
      ('run (run)))
    ((leds 'change-leds!) (list-leds))
    (when (eq? msg 'finished?)
      (eq? state-term state)))
    
  dispatch))






