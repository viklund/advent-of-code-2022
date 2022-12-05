#lang racket

(define in (open-input-file "05.input"))
;(define in (open-input-file "05.sample"))

;; Pick out only the name of the box in the stack, the string for each box is 4
;; characters wide "[N] ". Where N is the name
(define (conver l) (let ([en  (lambda (n) (+ (* 4 n) 1))]
                         [max (/ (+ 1 (string-length l)) 4)])
                     (map (lambda (x) (string-ref l (en x))) (range max))))

(define (is-space c) (= (char->integer c) 32))

;; Parse the starting stacks from the file
(define stacks
  (let* ([pre (drop-right (for/list ([l (in-lines in)])
                                    #:break (= (string-length l) 0) ;; Rest of the file is orders
                                    (conver l))
                          1)]
         [num (length (car pre))])
    (for/list ([stack-n (range num)])
              (filter (lambda (n) (not (is-space n)))
                      (for/list ([idx (range (length pre))])
                                (list-ref (list-ref pre idx) stack-n))))))


(struct move (number from to))

(define (parse-line s)
  (match-let* ([(list m number-s f from-ns t to-ns) (string-split s " ")]
               [from-n                              (- (string->number from-ns) 1)]
               [to-n                                (- (string->number to-ns) 1)]
               [number                              (string->number number-s)])
             (move number from-n to-n)))

(define (make-move1 m s)
  (let* ([from       (list-ref s (move-from m))]
         [to         (list-ref s (move-to m))]
         [elems-move (take from (move-number m))]
         [from-new   (drop from (move-number m))]
         [to-new     (append (reverse elems-move) to)])
        (list-set
          (list-set s (move-from m) from-new)
          (move-to m) to-new)))

(define (make-move2 m s)
  (let* ([from       (list-ref s (move-from m))]
         [to         (list-ref s (move-to m))]
         [elems-move (take from (move-number m))]
         [from-new   (drop from (move-number m))]
         [to-new     (append elems-move to)])
        (list-set
          (list-set s (move-from m) from-new)
          (move-to m) to-new)))

;; Run the program
(match-let ([orders (port->lines in)]
            [show   (lambda (x) (displayln (list->string x)))])
    (show (map car (foldl make-move1 stacks (map parse-line orders))))
    (show (map car (foldl make-move2 stacks (map parse-line orders)))))
