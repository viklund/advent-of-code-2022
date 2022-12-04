#lang racket


(define file-contents
  (port->string (open-input-file "04.input") #:close? #t))

(define (by-line f) (string-split f "\n"))

(define (sum l)
  (foldl + 0 l))


(struct segment (start stop))

(define (make-segment-string start stop)
  (segment (string->number start) (string->number stop)))

(define (parse-line line)
  (match-let* ([(list a b) (string-split line ",")]
               [(list a-start a-end) (string-split a "-")]
               [(list b-start b-end) (string-split b "-")])
              (cons (make-segment-string a-start a-end)
                    (make-segment-string b-start b-end))))

(define (contains l)
  (match-let ([(cons s1 s2) l])
             (cond
               [(and (<= (segment-start s1) (segment-start s2))
                     (>= (segment-stop  s1) (segment-stop  s2))) #t] 
               [(and (>= (segment-start s1) (segment-start s2))
                     (<= (segment-stop  s1) (segment-stop  s2))) #t] 
               [else #f])))

(define (overlaps l)
  (match-let ([(cons s1 s2) l])
             (cond
               [(and (>= (segment-start s1) (segment-start s2))
                     (<= (segment-start s1) (segment-stop  s2))) #t] 
               [(and (>= (segment-stop  s1) (segment-start s2))
                     (<= (segment-stop  s1) (segment-stop  s2))) #t] 
               [(and (>= (segment-start s2) (segment-start s1))
                     (<= (segment-start s2) (segment-stop  s1))) #t] 
               [else #f])))


(length (filter identity (map (compose contains parse-line) (by-line file-contents))))
(length (filter identity (map (compose overlaps parse-line) (by-line file-contents))))
