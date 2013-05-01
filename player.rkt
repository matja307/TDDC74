(define (make-player color ai)
  
  (define bricks 30)
  
  (define (countdown-bricks self)
    (set! bricks (- bricks 1)))
  
  (define (possible-score-list self)
    (let ((return-lst '()))
      (define (iter  lst)
        (cond ((null? lst) return-lst)
              (else  
               (set! return-lst (cons (cons (ask *board* 'count-changing-bricks (car (car lst)) (cdr (car lst)) color) (car lst)) return-lst))
               (iter (cdr lst)))))
      (iter (ask *board* 'valid-moves-list color))))
  
  (define (calculate-move self)
    (let ((pivot 0) 
          (pos '()))
      (define (iter lst)
        (cond ((null? lst) pos)
              ((> (car (car lst)) pivot)
               (set! pivot (car (car lst)))
               (set! pos (cdr (car lst)))
               (iter (cdr lst)))
              (else (iter (cdr lst)))))
      (iter (ask self 'possible-score-list))))
  
  
  (define (self msg)
    (cond
      ((eq? msg 'color) (lambda (self) color))
      ((eq? msg 'bricks) (lambda (self) bricks))
      ((eq? msg 'ai?) (lambda (self) ai))
      ((eq? msg 'calculate-move) calculate-move)
      ((eq? msg 'possible-score-list) possible-score-list)
      ((eq? msg 'countdown-bricks) countdown-bricks)))
  
  self)

(define player-1 (make-player 1 #f))
