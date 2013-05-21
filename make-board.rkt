(load "matrix.rkt")

(define (make-board)
  
  ;Board matrix
  (define matrix (make-matrix))
    
  ;; proc: put-brick!
  ;; param: (board) self, (int) x, (int) y, (int) player <color value>
  ;; return: (void)
  ;; Changes matrix to account for a brick on coordinate x,y in color player
  (define (put-brick! self x y player)
    (ask matrix 'set-cell! x y player))
  
  ;; proc: count-bricks
  ;; param: (board) self
  ;; return: (pair) (#black . #white)
  ;; Returns a pair with the number of black and white bricks on the board
  (define (count-bricks self)
    (cons (ask matrix 'count-cells 0) (ask matrix 'count-cells 1)))
  
  ;; proc: print-matrix
  ;; param: (board) self
  ;; return: (void)
  ;; Prints a represenatation of the matrix
  (define (print-matrix self)
    (ask matrix 'print))
  
  ;; proc: list-of-changing-bricks
  ;; param: (board) self, (int) x, (int) y, (int) player-color
  ;; return: (list) ((x1 . y1) ... (xn . yn))
  ;; Returns a list of the coordinates of all brics that 
  ;; would flip if a brick of color 'player-color' would
  ;; be placed on coordinate 'x','y'.
  (define (list-of-changing-bricks self x y player-color)
    (let ((result '())
          (temp-list '()))
      (define (count direction current)
        
        (let ((next (cons (+ (car current) (car direction)) 
                          (+ (cdr current) (cdr direction)))))
          
          (cond ((or (= (car next) 8) 
                     (= (cdr next) 8)
                     (= (car next) -1)
                     (= (cdr next) -1))
                 #f)
                ((not (ask matrix 'matrix-ref (car current) (cdr current))) 
                 #f)
                ((eq? (ask matrix 'matrix-ref (car current) (cdr current)) player-color) 
                 #f)
                ((eq? (ask matrix 'matrix-ref (car next) (cdr next)) player-color)
                 (set! temp-list (cons current temp-list))
                 (set! result (append temp-list result)))
                (else
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

  ;; proc: count-changing-bricks
  ;; param: (board) self, (int) x, (int) y, (int) player-color
  ;; return: (int) 
  ;; Returns the number of bricks that would flip if a brick of color
  ;; 'player-color' would be placed on coordinate 'x','y'.
  (define (count-changing-bricks self x y player-color)
    (length (list-of-changing-bricks self x y player-color)))
  
  ;; proc: valid-move?
  ;; param: (board) self, (int) x, (int) y, (int) player-color
  ;; return: (bool) 
  ;; Returns a boolean of whether a move on coordinate 'x','y' of color
  ;; 'player-color' is a valid move.
  (define (valid-move? self x y player-color)
    (and (not (ask matrix 'matrix-ref x y)) 
             (> (ask self 'count-changing-bricks x y player-color) 0)))

  ;; proc: valid-moves-list
  ;; param: (board) self, (int) player-color
  ;; return: (list) ((x1 . y1) ... (xn, yn)) 
  ;; Returns a list of all valid moves of color 'player-color
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
  
  ;; proc: flip!
  ;; param: (board) self, (int) x, (int) y, (int) player-color
  ;; return: (void) 
  ;; Changes matrix and updates GUI with a brick on coordinates 'x','y'
  ;; of color 'player-color', aswell as all other bricks flipped by this 
  ;; move.
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