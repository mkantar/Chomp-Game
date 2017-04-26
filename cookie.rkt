#lang racket

(require 2htdp/image)

(define eat?
  (lambda (elm)
         (if (= elm 0) #t
             #f)))

(define remains?
  (lambda (lst)
    (cond ((empty? lst) #t)
          ((equal? (car lst) 1) #f)
          (else (remains? (cdr lst))))))

(define game-over?
  (lambda (lst)
    (if (empty? lst) #t
        (and (remains? (car lst)) (game-over? (cdr lst))))))

(define size
  (lambda (lst)
    (if (empty? lst) 0 (+ 1 (size (cdr lst))))))

(define navigate
  (lambda (lst coordinates)
    (define (get-y)
      (let loop ((l lst) (coordinate (car (cdr coordinates))))
        (if (equal? coordinate 0) (car l)
              (loop (cdr l) (- coordinate 1)))
        ))
      (define (get-x)
        (let loop ((l (get-y)) (coordinate (car coordinates)))
          (if (equal? coordinate 0) (car l)
              (loop (cdr l) (- coordinate 1)))))
    (get-x)))

(define catch-exception
  (lambda (lst coordinates)
    (cond ((or (symbol? (car coordinates)) (symbol? (car (cdr coordinates))))
           1)
          ((or(> (car (cdr coordinates)) (- (size lst) 1)) (negative? (car (cdr coordinates)))); y check negative
           2)
          ((or (> (car coordinates) (- (size (car lst)) 1)) (negative? (car coordinates))); x
           3)
          ((and (equal? (car coordinates) 0) (equal? (car (cdr coordinates)) 0))
           4)
          ((eat? (navigate lst coordinates))
           5))))

(define eat-cookies 
  (lambda (lst coordinates)
      (let loop ((l lst) (count 0))
        (cond ((equal? (size lst) count) '())
              ((< count (car (cdr coordinates)))
               (cons (car l) (loop (cdr l) (+ count 1))))
              (else                  ; (equal? count (car (cdr coordinates)))
               (cons
                (let eat-right ((sl (car l)) (counter 0))
                  (cond ((equal? counter (size (car l)))
                         '())
                        ((< counter (car coordinates))
                         (cons (car sl) (eat-right (cdr sl) (+ counter 1))))
                        (else (cons 0 (eat-right (cdr sl) (+ counter 1))))))
                (loop (cdr l) (+ count 1))))))))
;              (else (cons
;                     (let eat-below ((sl (car l)) (counter 0))
;                       (cond ((equal? counter (size (car l)))
;                              '())
;                             ((equal? counter (car coordinates))
;                              (cons 0 (eat-below (cdr sl) (+ counter 1))))
;                             (else (cons (car sl) (eat-below (cdr sl) (+ counter 1))))))
;                     (loop (cdr l) (+ count 1))))))))

(define user-turn
  (lambda (lst)
    (display (text "Choose a cookie!" 18 "DarkSlateGray"))
    (newline)
    (define coordinates '())
    (let ((co-list '()))
      (display (text "x: " 18 "DarkSlateGray"))
      (let ((x (read)))
        (set! co-list (append co-list (list x))))
      (display (text "y: " 18 "DarkSlateGray"))
      (let ((y (read)))
        (set! co-list (append co-list (list y))))
      (set! coordinates co-list))

    (let ((error-code (catch-exception lst coordinates)))
      (cond ((equal? error-code 1)
             (begin
               (display (text/font "Invalid input entered!" 18 "red" #f 'roman 'italic 'normal #f))
               (newline)
               (user-turn lst)))
            ((equal? error-code 2)
             (begin
               (display (text/font "y is out of bound!" 18 "red" #f 'roman 'italic 'normal #f))
               (newline)
               (user-turn lst)))
            ((equal? error-code 3)
             (begin
               (display (text/font "x is out of bound!" 18 "red" #f 'roman 'italic 'normal #f))
               (newline)
               (user-turn lst)))
            ((equal? error-code 4)
             (begin
               (display (text/font "Still exists cookie(s)!" 18 "red" #f 'roman 'italic 'normal #f))
               (newline)
               (user-turn lst)))
            ((equal? error-code 5)
             (begin
               (display (text/font "No cookie exists at the choosen coordinates!" 18 "red" #f 'roman 'italic 'normal #f))
               (newline)
               (user-turn lst)))
            (else (eat-cookies lst coordinates))))))

(define computer-turn
  (lambda (lst)
    (sleep 0.5)
         (let ((x (random (size (car lst)))) (y (random (size lst))))
           (let ([coordinates (list x y)])
             (let ([error-code (catch-exception lst coordinates)])
               (if (or (equal? error-code 4) (equal? error-code 5))
                   (begin
                     (display (text "." 18 "black"))
                   (computer-turn lst))
                   (begin
                     (newline)
                     (display (text "Computer has moved" 18 "DarkSlateGray"))
                     (newline)
                     (eat-cookies lst coordinates))))))))

(provide eat?)
(provide game-over?)
(provide user-turn)
(provide computer-turn)

                 





