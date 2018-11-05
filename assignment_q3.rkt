(define (node middle left right)
  (list middle left right)
)

(define (root node)
  (car node)
)

(define (left_subtree node)
  (cadr node)
)

(define (right_subtree node)
  (caddr node)
)

;A
(define (traverse tree)
  (if(null? tree)
    '()
    (begin
      (traverse (left_subtree tree))
      (display (root tree))(newline)
      (traverse (right_subtree tree))
    )
  )
)


;B
(define (search x tree)
  (if (null? tree)
    #f
    (let ((current (root tree)))
      (cond
        [(< x current)(search x (left_subtree tree))]
        [(> x current)(search x (right_subtree tree))]
        [(equal? x current) #t ]
      )
    )
  )
)


;C
(define (insert x tree)
  (if (null? tree)
   (node x '() '())
   (let ((current (root tree)))
      (cond
        [(< x current)(node current (insert x (left_subtree tree)) (right_subtree tree) )]
        [(> x current)(node current (left_subtree tree) (insert x (right_subtree tree)))]
        [(equal? x current)(display "Item Found")]
      )
    )
  )
)


;D
(define (insert_list list tree)
  (if (empty? list) tree
    (insert_list (cdr list) (insert (car list) tree)))
)


;E
(define (t_sort list)
  (traverse (insert_list list '() ))
)


;F
(define (insert_list_desc x tree)
  (if (null? tree)
    (node x '() '())
    (let ((current (root tree)))
      (cond
        [(> x current)(node current (insert_list_desc x (left_subtree tree)) (right_subtree tree) )]
        [(< x current)(node current (left_subtree tree) (insert_list_desc x (right_subtree tree)))]
        [(equal? x current)(display "Item Found")]
      )
    )
  )
)

(define (insert_list_ld x tree)
  (if (null? tree)
    (node x '() '())
    (let ((current (root tree)))
      (cond
        [(< (remainder x 10)(remainder current 10))(node current (insert_list_ld x (left_subtree tree)) (right_subtree tree) )]
        [(> (remainder x 10)(remainder current 10))(node current (left_subtree tree) (insert_list_ld x (right_subtree tree)))]
        [(equal? x current)(display "Item Found")]
      )
    )
  )
)

(define (insert_list_d list tree)
  (if (empty? list) tree
    (insert_list_d (cdr list) (insert_list_desc (car list) tree)))
)

(define (insert_list_last_digit list tree)
  (if (empty? list) tree
    (insert_list_last_digit (cdr list) (insert_list_ld (car list) tree)))
)

(define (descending list)
  (traverse (insert_list_d list '()))
)

(define (ascending list)
  (traverse (insert_list list '()))
)

(define (ascending_last_digit list)
  (traverse (insert_list_last_digit list '()))
)

(define (higher_order_sort list function)
  (function list)
)