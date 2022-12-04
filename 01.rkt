#lang racket


(define file-contents
  (port->string (open-input-file "01.input") #:close? #t))

(define (sum l)
  (foldl + 0 l))

(define (by-section f) (string-split f "\n\n"))
(define (by-line f) (string-split f "\n"))

(define (biggest l)
  (foldl (lambda (a b) (if (> a b) a b)) 0 l))

(define (sum-section s)
  (sum (map string->number (by-line s))))

(biggest (map sum-section (by-section file-contents)))

(sum (take (sort (map sum-section (by-section file-contents))
                 >)
           3))
