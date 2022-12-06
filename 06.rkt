#lang racket

(require "util.rkt")

(define all (file-contents "06.input"))

(define (all-different-n n s pos)
  (let* ([t (substring s pos (+ pos n))])
    (= n (length (remove-duplicates (string->list t))))))

(displayln (+  4 (car (filter (lambda (x) (all-different-n  4 all x)) (range (- (string-length all)  3))))))
(displayln (+ 14 (car (filter (lambda (x) (all-different-n 14 all x)) (range (- (string-length all) 13))))))
