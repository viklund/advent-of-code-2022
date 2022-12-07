#lang racket

(require "util.rkt")
(define f "07.input")

(define in (open-input-file f))


(define (make-command l)
  (cond
    [(string=? "ls" (car l)) (list "ls")]
    [(string=? "cd" (car l)) (list "cd" (cadr l))]))

(define (parse-line l)
  (match-let ([(list first rest ...) (string-split l)])
             (cond
               [(string=? first "$")   (make-command rest)]
               [(string=? first "dir") (list "dir" (car rest))]
               [else                   (list "file" (string->number first))])))

(define state (make-hash))

(for ([l (in-lines in)])
     (let ([r (parse-line l)])
       (case (car r)
         [("ls")   (displayln "ls")] ; really a no-op
         [("cd")   (displayln "cd")]
         [("dir")  (displayln "dir")]
         [("file") (displayln (cadr r))]
         [else    (displayln "WTF")]
          )))
