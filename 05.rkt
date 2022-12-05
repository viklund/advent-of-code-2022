#lang racket

(require "util.rkt")
(define f "05.input")


;[Q] [J]                         [H]
;[G] [S] [Q]     [Z]             [P]
;[P] [F] [M]     [F]     [F]     [S]
;[R] [R] [P] [F] [V]     [D]     [L]
;[L] [W] [W] [D] [W] [S] [V]     [G]
;[C] [H] [H] [T] [D] [L] [M] [B] [B]
;[T] [Q] [B] [S] [L] [C] [B] [J] [N]
;[F] [N] [F] [V] [Q] [Z] [Z] [T] [Q]
; 1   2   3   4   5   6   7   8   9 

; I should reverse all of them, so the top element is first
(define stacks
  (map reverse
    (list '(F T C L R P G Q)
          '(N Q H W R F S J)
          '(F B H W P M Q)
          '(V S T D F)
          '(Q L D W V F Z)
          '(Z C L S)
          '(Z B M V D F)
          '(T J B)
          '(Q N B G L S P H))))

;    [D]    
;[N] [C]    
;[Z] [M] [P]
; 1   2   3 

;(define stacks
;  (map reverse
;    (list '(Z N)
;          '(M C D)
;          '(P))))

;(define (move order s)
;  (match-let* ([(list m number-s f from-ns t to-ns) (string-split order " ")]
;               [from-n                          (- (string->number from-ns) 1)]
;               [to-n                            (- (string->number to-ns) 1)]
;               [number                          (string->number number-s)]
;               [from                            (list-ref s from-n)]
;               [to                              (list-ref s to-n)]
;               [(list elems-move from-new)      (split-at from number)]
;               [to-new                          (append elems-move to)])
;              (list-set 
;                (list-set s from-n from-new)
;                to-n to-new)))

(define (move order s)
  (match-let* ([(list m number-s f from-ns t to-ns) (string-split order " ")]
               [from-n                          (- (string->number from-ns) 1)]
               [to-n                            (- (string->number to-ns) 1)]
               [number                          (string->number number-s)]
               [from                            (list-ref s from-n)]
               [to                              (list-ref s to-n)]
               [elems-move                      (take from number)]
               [from-new                        (drop from number)]
               [to-new                          (append (reverse elems-move) to)])
              (list-set 
                (list-set s from-n from-new)
                to-n to-new)))

;(match-let ([(list stacks orders) (by-section (file-contents f))])
;    orders)

;(move stacks)

;(move "" stacks)
;stacks
;(move "move 1 from 2 to 1" stacks)
;(move "move 3 from 1 to 3" (move "move 1 from 2 to 1" stacks))
;move 2 from 2 to 1
;move 1 from 1 to 2

(match-let ([(list s orders) (by-section (file-contents f))])
    (map car (foldl move stacks (by-line orders))))
