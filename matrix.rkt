(define (empty-matrix)
  (list (list #f #f #f #f #f #f #f #f)
        (list #f #f #f #f #f #f #f #f)
        (list #f #f #f #f #f #f #f #f)
        (list #f #f #f #f #f #f #f #f)
        (list #f #f #f #f #f #f #f #f)
        (list #f #f #f #f #f #f #f #f)
        (list #f #f #f #f #f #f #f #f)
        (list #f #f #f #f #f #f #f #f)))

(define (matrix-ref mx x y)
  (list-ref (list-ref mx y) x))

(define (matrix-change-cell mx x y val)
  (define (iter-x xi yi)
    (cond ((= xi 8) '())
          ((= xi x) (cons val (iter-x (+ xi 1) yi)))
          (else (cons (matrix-ref mx xi yi) (iter-x (+ xi 1) yi)))))
  
  (define (iter-y yi)
    (cond ((= yi 8) '())
          ((= yi y) (cons (iter-x 0 yi) (iter-y (+ yi 1))))
          (else (cons (list-ref mx yi) (iter-y (+ yi 1))))))
  
  (iter-y 0))

(define (matrix-display mx)
  (define (iter mx-left)
    (if (null? (cdr mx-left))
        (begin
          (display (car mx-left))(newline))
        (begin
          (display (car mx-left))(newline)
          (iter (cdr mx-left)))))
  
  (iter mx))

(define (matrix-count mx val)
  (define (iter-x list-left)
    (cond ((null? list-left) 0)
          ((eq? val (car list-left)) (+ 1 (iter-x (cdr list-left))))
          (else (+ 0 (iter-x (cdr list-left))))))
  
  (define (iter-y list-left)
    (if (null? list-left) 
        0
        (+ (iter-x (car list-left)) (iter-y (cdr list-left)))))
  
  (iter-y mx))


(define tstmatrix (empty-matrix))






