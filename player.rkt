(define (make-player color ai)
  
  ;a player is fitted with a finite set of bricks
  (define bricks 30)
  
  ;;evaluation weights of each square in the board
  (define weight-matrix                      
    (list (list 100 -20 20  5  5 20 -20 100)
          (list -20 -50 -5 -5 -5 -5 -50 -20)
          (list  20 -5  15  1  1 15  -5  20)
          (list   5 -5   1  1  1  1  -5   5)
          (list   5 -5   1  1  1  1  -5   5)
          (list  20 -5  15  1  1 15  -5  20)
          (list -20 -50 -5 -5 -5 -5 -50 -20)
          (list 100 -20 20  5  5 20 -20 100)))
  
  (define (countdown-bricks self) 
    (set! bricks (- bricks 1)))
  
  ;;returns value of position (x,y) in the matrix
  (define (weight x y) 
    (list-ref (list-ref weight-matrix y) x))
  
  ;;calculates the sum of the weights from the layed and fliped bricks,
  ;;when a brick is layed in position (x,y) 
  (define (possible-score self x y color) 
    (let((list-of-changing-bricks (ask *board* 'list-of-changing-bricks x y color))
         (score 0))
      (define (iter lst)
        (cond((null? lst)
              (set! score (+ score (weight x y)))
              score)
             (else
              (set! score ( + (weight (car(car lst)) (cdr (car lst))) score))
              (iter (cdr lst)))))
      (iter list-of-changing-bricks)))
  
  ;;appending the individual score to its position and returning a list according to:
  ;; '((score-1 x-1 y-1) (score-2 x-2 y-2) ....)
  (define (possible-score-list self)  
    (let ((return-lst '()))
      (define (iter  lst)
        (cond ((null? lst) return-lst)
              (else  
               (set! return-lst (cons (cons (possible-score self (car (car lst)) (cdr (car lst)) color) (car lst)) return-lst))
               (iter (cdr lst)))))
      (iter (ask *board* 'valid-moves-list color))))
  
  ;; returns the position that gives maximum score in the possible-score-list (above)
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
      ((eq? msg 'possible-score) possible-score)
      ((eq? msg 'possible-score-list) possible-score-list)
      ((eq? msg 'countdown-bricks) countdown-bricks)))
  
  self)

(define player-1 (make-player 1 #f))
