(define (make-matrix)
  
  ;; Starting matrix
  (define mx
    (list (list #f #f #f #f #f #f #f #f)
          (list #f #f #f #f #f #f #f #f)
          (list #f #f #f #f #f #f #f #f)
          (list #f #f #f #f #f #f #f #f)
          (list #f #f #f #f #f #f #f #f)
          (list #f #f #f #f #f #f #f #f)
          (list #f #f #f #f #f #f #f #f)
          (list #f #f #f #f #f #f #f #f)))
  
  ;; proc: matrix-ref
  ;; param: (matrix) self, (int) x, (int) y
  ;; return: (?) 
  ;; Returns the content of cell with coordinate
  ;; 'x','y' in mx. 
  (define (matrix-ref self x y)
    (if (or (>= x 8) (>= y 8) (< x 0) (< y 0))
        #f
        (list-ref (list-ref mx y) x)))
  
  ;; proc: set-cell!
  ;; param: (matrix) self, (int) x, (int) y, (?) val
  ;; return: (void) 
  ;; Updates mx to hold value 'val' in cell 'x','y'.
  (define (set-cell! self x y val)
    (define (iter-x xi yi)
      (cond ((= xi 8) '())
            ((= xi x) (cons val (iter-x (+ xi 1) yi)))
            (else (cons (matrix-ref mx xi yi) (iter-x (+ xi 1) yi)))))
    
    (define (iter-y yi)
      (cond ((= yi 8) '())
            ((= yi y) (cons (iter-x 0 yi) (iter-y (+ yi 1))))
            (else (cons (list-ref mx yi) (iter-y (+ yi 1))))))
    
    (set! mx (iter-y 0)))
  
  ;; proc: print
  ;; param: (matrix) self
  ;; return: (void) 
  ;; Prints a representation of mx
  (define (print self)
    (define (iter mx-left)
      (if (null? (cdr mx-left))
          (begin
            (display (car mx-left))(newline))
          (begin
            (display (car mx-left))(newline)
            (iter (cdr mx-left)))))
    
    (iter mx))
  
  ;; proc: count-cells
  ;; param: (matrix) self, (?) val
  ;; return: (int)  
  ;; Returns a count of the instances of 'val' in mx.
  (define (count-cells self val)
    (define (iter-x list-left)
      (cond ((null? list-left) 0)
            ((eq? val (car list-left)) (+ 1 (iter-x (cdr list-left))))
            (else (+ 0 (iter-x (cdr list-left))))))
    
    (define (iter-y list-left)
      (if (null? list-left) 
          0
          (+ (iter-x (car list-left)) (iter-y (cdr list-left)))))
    
    (iter-y mx))

  
  (define (self msg)
    (cond
      ((eq? msg 'matrix-ref) matrix-ref)
      ((eq? msg 'set-cell!) set-cell!)
      ((eq? msg 'print) print)
      ((eq? msg 'count-cells) count-cells)))
  
  self)








