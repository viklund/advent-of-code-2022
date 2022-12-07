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

; State is like this
; (hash "current-dir" . "Dir"
;       "<-dir->"     . number)

(define (clean-dir dir)
    (string-join (string-split dir "/" #:trim? #t #:repeat? #t) "/"))

(define (add-file-size state num)
    (hash-update state (hash-ref state "current-dir") (curry + num)))

(define (up-dir state)
  (let* ([current-dir (hash-ref state "current-dir")]
         [dir-size    (hash-ref state current-dir)]
         [new-dir     (string-join (drop-right (string-split current-dir "/" #:trim? #t #:repeat? #t) 1) "/")]
         [new-state   (hash-update state new-dir (curry + dir-size))])
    (hash-set new-state "current-dir" new-dir)))

(define (enter-dir state dir)
  (let* ([current-dir (hash-ref state "current-dir")]
         [new-dir (clean-dir (string-join (list current-dir dir) "/"))]
         [st      (hash-set state new-dir 0)])
    (hash-set st "current-dir" new-dir)))

(define (perform-command command state)
  (case (car command)
    [("ls" "dir")   state] ; no-ops, just return the state
    [("file")       (add-file-size state (cadr command))]
    [("cd")         (if (equal? (cadr command) "..") (up-dir state) (enter-dir state (cadr command)))]))


(define res
  (for/fold
    ([st (hash "current-dir" "")])
    ([l (in-lines in)])
    (perform-command (parse-line l) st)))

(sum (filter (lambda (x) (and (number? x) (< x 100000))) (hash-values res)))

(define free-space (- 70000000 (hash-ref res "")))

(define (last-elem l)
  (if (null? (cdr l)) (car l) (last-elem (cdr l))))

(last-elem (filter (lambda (x) (< 30000000 (+ free-space x))) (sort (filter (curry number?) (hash-values res)) >)))
