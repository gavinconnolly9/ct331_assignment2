#lang racket
(provide ins_beg)
(provide ins_end)
(provide count_top_level)
(provide count_instances)
(provide count_instances_tr)
(provide count_instances_deep)


;Part A:  
(define (ins_beg el lst)
  (append (list el) lst)
)


;Part B:
(define (ins_end el lst)
  (append lst (list el))
)


;Part C:
(define (count_top_level lst)
  (length lst)
)


;Part D: Non-Tail Recursive
(define (count_instances el lst)
  (c_instances el lst 0)
)

(define (c_instances el lst total)
  (cond
  [(null? lst) total]
  [(= el (car lst)) (c_instances el (cdr lst) (+ 1 total))]
  [else (c_instances el (cdr lst) total)]
  )
)


;Part E: Tail Recursive
(define (count_instances_tr el lst)
  (cond
    [(null? lst) 0] ;if list is empty then return 0
    ;if el is equal to first item in list add 1 to the count_instances of cdr
    [(= el (car lst)) (+ 1 (count_instances_tr el (cdr lst)))]
    ;else add nothing
    [else (count_instances_tr el(cdr lst)) ]
  )
)


;Part F
(define (count_instances_deep el lst)
  (c_instances_deep el lst 0)
)

(define (c_instances_deep el lst total)
  (cond
    [(null? lst) total] ;if list is empty then return is 0
    ;;if first element is a list do count instances in that
    [(list? (car lst))(c_instances_deep el (cdr lst) (c_instances el (car lst) total))]
    [(= el (car lst)) (c_instances_deep el (cdr lst) (+ 1 total))]
    [else (c_instances_deep el(cdr lst) total) ]
  )
)