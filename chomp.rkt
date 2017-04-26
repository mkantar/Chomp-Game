#lang racket

(require 2htdp/image)
(require "cookie.rkt")

(define display-row
  (lambda (lst)
    (let loop ((l lst) (count (length lst)))
      (cond ((equal? count 1)
             (if (eat? (car l)) (circle 20 "solid" "gray") (circle 20 "solid" "brown")))
            (else (beside
                   (if (eat? (car l)) (circle 20 "solid" "gray") (circle 20 "solid" "brown"))
                   (rectangle 20 10 "solid" "gray")
                   (loop (cdr l) (- count 1))))))))

(define display-table
  (lambda (lst)
    (let loop ((l lst) (counter (length lst)))
      (cond ((equal? counter 0) (display ""))
            ((equal? counter (length lst))
             (begin
               (let ([first-row (beside (circle 20 "solid" "black") (beside (rectangle 20 10 "solid" "gray") (display-row (cdar l))))])
               (display (underlay (rectangle 300 40 "solid" "gray") first-row)))
               (newline)
               (loop (cdr l) (- counter 1))))
            (else
             (begin
               (display (underlay (rectangle 300 40 "solid" "gray") (display-row (car l))))
               (newline)
               (loop (cdr l) (- counter 1))))))))

                                                                                                         
(define chomp
  (lambda (lst turn)
    (display-table lst)
    (newline)
   (cond ((game-over? (cons (cdar lst) (cdr lst)))
          (if (equal? turn 'user) (display (text "You Lost!" 36 "DarkRed")) (display (text "You Won!" 36 "RoyalBlue"))))
         ((equal? turn 'user)
          (begin
            (display (text "Your turn!" 24 "Magenta"))
            (newline)
            (chomp (user-turn lst) 'computer)))
         (else
          (begin
            (display (text "Computer turn!" 24 "Magenta"))
            (newline)
            (chomp (computer-turn lst) 'user)))
       )))

(define (init)
  (let ([rand (random 2)])
    (if (equal? rand 0) (chomp '((1 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1)) 'user) (chomp '((1 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1)) 'computer))))

(init)
  
