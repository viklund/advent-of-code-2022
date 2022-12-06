#lang racket

(require "util.rkt")

(define all (file-contents "06.input"))

(define (all-different-n n s pos)
  (let* ([t (substring s pos (+ pos n))])
    (= n (length (remove-duplicates (string->list t))))))

(define (first-instance-n n l)
  (+ n (findf (curry all-different-n n l) (range (- (string-length l) (dec n))))))

(first-instance-n 4 all)
(first-instance-n 14 all)
