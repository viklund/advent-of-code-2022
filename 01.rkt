#lang racket


(define file-contents
  (port->string (open-input-file "01.input") #:close? #t))

(define (sum l)
  (foldl + 0 l))

(displayln (foldl (lambda (a b) (if (> a b) a b))
                0
                (map (lambda (l)
                       (sum (map (lambda (n) (string->number n)) 
                                       (string-split l "\n"))))
                     (string-split file-contents "\n\n"))))

(displayln (sum (take (sort (map (lambda (l)
                              (foldl + 0 (map (lambda (n) (string->number n)) 
                                              (string-split l "\n"))))
                            (string-split file-contents "\n\n"))
                       >)
                 3)))
