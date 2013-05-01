(load "matrix.rkt")

(define (make-board)
  
  (define matrix (make-matrix))
    
  (define (put-brick! self x y player)
    (ask matrix 'set-cell! x y player))
  
  (define (count-bricks self)
    (cons (ask matrix 'count-cells 0) (ask matrix 'count-cells 1)))
  
  (define (print-matrix self)
    (ask matrix 'print))
  
  
  (define (list-of-changing-bricks self x y player-color)
    (let ((result '())
          (temp-list '()))
      (define (count direction current)
        
        (let ((next (cons (+ (car current) (car direction)) 
                          (+ (cdr current) (cdr direction)))))
          
          (cond ((or (= (car next) 8) ;;Nästa ruta utanför spelplanen
                     (= (cdr next) 8)
                     (= (car next) -1)
                     (= (cdr next) -1))
                 #f)
                ((not (ask matrix 'matrix-ref (car current) (cdr current))) ;;Nuvarande bricka tom
                 #f)
                ((eq? (ask matrix 'matrix-ref (car current) (cdr current)) player-color) ;;Nuvarande bricka min färg       
                 #f)
                ((eq? (ask matrix 'matrix-ref (car next) (cdr next)) player-color) ;;Nästa bricka min färg       
                 (set! temp-list (cons current temp-list))
                 (set! result (append temp-list result)))
                (else ;;Nubvarande bricka motståndarens färg
                 (set! temp-list (cons current temp-list))
                 (count direction next)))))
      
      (count (cons 1 0) (cons (+ x 1) (+ y 0)))
      (set! temp-list '())
      (count (cons 1 1) (cons (+ x 1) (+ y 1)))
      (set! temp-list '())
      (count (cons 0 1) (cons (+ x 0) (+ y 1)))
      (set! temp-list '())
      (count (cons -1 1) (cons (+ x -1) (+ y 1)))
      (set! temp-list '())
      (count (cons -1 0) (cons (+ x -1) (+ y 0)))
      (set! temp-list '())
      (count (cons -1 -1) (cons (+ x -1) (+ y -1)))
      (set! temp-list '())
      (count (cons 0 -1) (cons (+ x 0) (+ y -1)))
      (set! temp-list '())
      (count (cons 1 -1) (cons (+ x 1) (+ y -1)))
      (set! temp-list '())
      
      result))

  
  (define (count-changing-bricks self x y player-color)
    (length (list-of-changing-bricks self x y player-color)))
  
  (define (valid-move? self x y player-color)
    (and (not (ask matrix 'matrix-ref x y)) 
             (> (ask self 'count-changing-bricks x y player-color) 0)))

  (define (valid-moves-list self player-color)
    (let ((lst '()))
      (define (iter-x x y)
        (cond ((and (= x 7) (ask self 'valid-move? x y player-color)) 
               (set! lst (cons (cons x y) lst)))
              ((= x 7)
               0)
              ((ask self 'valid-move? x y player-color)
               (set! lst (cons (cons x y) lst))
               (iter-x (+ x 1) y))
              (else (iter-x (+ x 1) y))))
      
      (define (iter-y y)
        (if (= y 7)
            (iter-x 0 y)
            (begin
              (iter-x 0 y)
              (iter-y (+ y 1)))))
      
      (iter-y 0)
      lst))
  
  (define (flip! self x y player-color)
    (define (iter lst)
      (let ((x (caar lst))
            (y (cdar lst)))
        (if (null? (cdr lst))
            (begin
              (ask self 'put-brick! x y player-color)
              (if (= player-color 0) 
                  (set-piece-at! x y 'BLACK)
                  (set-piece-at! x y 'WHITE)))
            (begin
              (ask self 'put-brick! x y player-color)
              (if (= player-color 0) 
                  (set-piece-at! x y 'BLACK)
                  (set-piece-at! x y 'WHITE))
              (iter (cdr lst))))))
    (let ((flip-list (list-of-changing-bricks self x y player-color)))
      (iter flip-list)))
  
  (define (self msg)
    (cond
      ((eq? msg 'get-matrix) (lambda (self) matrix))
      ((eq? msg 'put-brick!) put-brick!)
      ((eq? msg 'count-bricks) count-bricks)
      ((eq? msg 'print-matrix) print-matrix)
      ((eq? msg 'list-of-changing-bricks) list-of-changing-bricks)
      ((eq? msg 'count-changing-bricks) count-changing-bricks)
      ((eq? msg 'valid-move?) valid-move?)
      ((eq? msg 'valid-moves-list) valid-moves-list)
      ((eq? msg 'flip!) flip!))) 
  
  self)