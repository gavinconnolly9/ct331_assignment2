#lang racket

(cons  1 2)

(cons 1(cons 2 ( cons 3 '() )))

(cons "Cons function" (cons 1 (cons (cons 2( cons 3( cons 4 '()))) '())))

(list "List function" 1 (list 2 3 4))

(append '("Append function") '(1) '((2 3 4)))

;list is just a function that uses the cons function and makes it easier to
;create lists rather than calling cons each time
;append can only take lists as arguments, can't take atoms