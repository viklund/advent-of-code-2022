#lang racket


(define file-contents
  (port->string (open-input-file "3.input") #:close? #t))

(define (sum l)
  (foldl + 0 l))

; "A" => 65
; "a" => 97
(define (value n)
  (let ([v (char->integer n)])
    (if (> v 96) (- v 96) (- v 38))))

(define (string->charl n)
  (map value (string->list n)))

(define (split-middle x)
    (let* ([n (/ (length x) 2)]
           [a (take x n)] 
           [b (drop x n)]) 
      (list a b)))

(displayln
  (sum (map (lambda (x)
              (let* ([d (split-middle x)]
                     [a (remove-duplicates (car d))]
                     [b (remove-duplicates (cadr d))])
                (check-duplicates (append a b)))) 
            (map string->charl
                 (string-split file-contents "\n")))))

(define (by-threes l)
  (if (empty? l) '()
    (cons (list (car l) (cadr l) (caddr l))
            (by-threes (drop l 3)))))

(define (check-tripplicates l)
  (let _inner ([look-for (car l)]
               [list     (cdr l)])
    (if (= 2 (count (lambda (e) (= look-for e)) list))
      look-for
      (_inner (car list) (cdr list)))))

(displayln
  (sum (map (lambda (tripple)
              (check-tripplicates (flatten (map remove-duplicates tripple))))
            (by-threes (map string->charl (string-split file-contents "\n"))))))
