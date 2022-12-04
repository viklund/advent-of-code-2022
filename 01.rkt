#lang racket

(require "util.rkt")
(define f "01.input")

(define (biggest l)
  (foldl (lambda (a b) (if (> a b) a b)) 0 l))

(define (sum-section s)
  (sum (map string->number (by-line s))))

(biggest (map sum-section (by-section (file-contents f))))

(sum (take (sort (map sum-section (by-section (file-contents f)))
                 >)
           3))
