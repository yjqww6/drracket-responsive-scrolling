#lang racket

(require racket/gui drracket/tool framework)

(provide tool@)

(define-unit tool@
  (import drracket:tool^)
  (export drracket:tool-exports^)

  (define phase1 void)
  (define phase2 void)
  
  (define m
    (mixin (canvas<%>) ()
      (define up 0)
      (define left 0)
      
      (define interval 100)
      
      (define timer #f)
      
      (define/override (on-char event)
        (define (start-timer)
          (unless timer
            (let loop ([int 20])
              (set!
               timer
               (new timer% [interval int] [just-once? #t]
                    [notify-callback
                     (λ ()
                       (cond
                         [(and (= up 0) (= left 0)) (set! timer #f)]
                         [else
                          (with-method ([wheel-step (this wheel-step)])
                            (define old-step (wheel-step))
            
                            (cond
                              [(> up 0)
                               (wheel-step (* up old-step))
                               (super on-char (new key-event% [key-code 'wheel-up]))]
                              [(< up 0)
                               (wheel-step (* (- up) old-step))
                               (super on-char (new key-event% [key-code 'wheel-down]))])
             
                            (cond
                              [(> left 0)
                               (wheel-step (* left old-step))
                               (super on-char (new key-event% [key-code 'wheel-left]))]
                              [(< left 0)
                               (wheel-step (* (- left) old-step))
                               (super on-char (new key-event% [key-code 'wheel-right]))])
                            (wheel-step old-step)
                            (set! up 0)
                            (set! left 0)
                            (loop interval))]))])))))
        
        (case (send event get-key-code)
          [(wheel-up) (set! up (+ up 1))
                      (start-timer)]
          [(wheel-down) (set! up (- up 1))
                        (start-timer)]
          [(wheel-left) (set! left (+ left 1))
                        (start-timer)]
          [(wheel-right) (set! left (- left 1))
                         (start-timer)]
          [else
           (when timer
             (send timer stop)
             (set! timer #f))
           (set! up 0)
           (set! left 0)
           (super on-char event)]))
      (super-new)))

  (drracket:get/extend:extend-definitions-canvas m)
  (drracket:get/extend:extend-interactions-canvas m))
