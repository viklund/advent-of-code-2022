#lang racket

(provide file-contents)
(provide sum)
(provide by-section)
(provide by-line)

(define (file-contents f)
  (port->string (open-input-file f) #:close? #t))

(define (sum l)
  (foldl + 0 l))

(define (by-section f) (string-split f "\n\n"))

(define (by-line f) (string-split f "\n"))
