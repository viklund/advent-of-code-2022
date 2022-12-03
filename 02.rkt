#lang racket


(define file-contents
  (port->string (open-input-file "2.input") #:close? #t))

(define (sum l)
  (foldl + 0 l))

(define (rock? a)
  (or (string=? a "A") (string=? a "X")))
(define (paper? a)
  (or (string=? a "B") (string=? a "Y")))
(define (scissor? a)
  (or (string=? a "C") (string=? a "Z")))

(define (value a)
  (cond
    [(rock? a) 1]
    [(paper? a) 2]
    [(scissor? a) 3]))

(define (score a b)
  (cond
    [(and (rock?    b) (rock?    a)) 3]
    [(and (paper?   b) (paper?   a)) 3]
    [(and (scissor? b) (scissor? a)) 3]

    [(and (rock?    b) (scissor? a)) 6]
    [(and (scissor? b) (paper?   a)) 6]
    [(and (paper?   b) (rock?    a)) 6]
    [else 0]))


(displayln (sum (map (lambda (l) 
                       (let* ([n (string-split l " ")]
                              [f (list-ref n 0)]
                              [l (list-ref n 1)])
                         (+ (value l) (score f l))))
                     (string-split file-contents "\n"))))

(define (outcome x)
  (cond
    [(string=? "X" x) 'lose]
    [(string=? "Y" x) 'draw]
    [(string=? "Z" x) 'win]))

(define (scorep other outcome)
  (cond
    [(and (rock?    other) (eq? outcome 'lose)) 3]  ; Scissors
    [(and (paper?   other) (eq? outcome 'lose)) 1]  ; Rock
    [(and (scissor? other) (eq? outcome 'lose)) 2]  ; Paper

    [(and (rock?    other) (eq? outcome 'draw)) 4]
    [(and (paper?   other) (eq? outcome 'draw)) 5]
    [(and (scissor? other) (eq? outcome 'draw)) 6]

    [(and (rock?    other) (eq? outcome 'win)) 8]
    [(and (paper?   other) (eq? outcome 'win)) 9]
    [(and (scissor? other) (eq? outcome 'win)) 7]
    ))

(displayln (sum (map (lambda (l)
                       (let* ([n (string-split l " ")]
                              [f (list-ref n 0)]
                              [l (list-ref n 1)])
                         (scorep f (outcome l))))
                     (string-split file-contents "\n"))))
