#lang racket


(define file-contents
  (port->string (open-input-file "3.input") #:close? #t))

(define (sum l)
  (foldl + 0 l))
