#lang racket
(define (binary_tree_leaf value )
  (list '() value '() )
)

(define (binary_tree_node left_node value right_node)
  (list left_node value right_node))
(define (traverse tree)
  (cond
    [(empty? tree) #t]
    [else
     (begin
         (traverse (car tree))
         (printf" ~a "(cadr tree))
         (traverse (caddr tree))
      )
    ]    
  )
)

(define test_tree
  (binary_tree_node
    (binary_tree_node
    (binary_tree_leaf 1)
    3
    (binary_tree_leaf 6)
    )
    8
    (binary_tree_node
    (binary_tree_leaf 9)
    10
    (binary_tree_leaf 11)
    )
  )
)
(define (search_tree tree value)
  (cond
    [(null? tree) #f]
    [(equal? value (cadr tree))]
    [(> (cadr tree) value)(search_tree (car tree) value)]
    [(< (cadr tree) value)(search_tree (caddr tree) value)]
  )
)
(define (insert_tree tree value)
  (display tree)
  (display "\n")
  (cond
    [(empty? tree)(binary_tree_leaf value)]
    [(= (cadr tree) value) tree]
    [(> (cadr tree) value)(list (insert_tree (car tree) value) (cadr tree) (caddr tree))]
    [(< (cadr tree) value)(list (car tree) (cadr tree) (insert_tree (caddr tree) value) )]        
  )
)
(define (insert_list_tree tree list)
  (cond
    [(empty? list) tree]
    [else (insert_list_tree (insert_tree tree (car list)) (cdr list))]
   )
)


(define (ascending_sort list)
  (traverse(insert_list_tree '() list))
)
(define (insert_descend_tree tree value)
  (cond
    [(empty? tree)(binary_tree_leaf value)]
    [(= (cadr tree) value) tree]
    [(< (cadr tree) value)(list (insert_descend_tree (caddr tree) value) (cadr tree) (car tree))]
    [(> (cadr tree) value)(list (caddr tree) (cadr tree) (insert_descend_tree (car tree) value) )]        
  )
)
(define (insert_list_descend_tree tree list)
  (cond
    [(empty? list) tree]
    [else (insert_list_descend_tree (insert_descend_tree tree (car list)) (cdr list))]
   )
)
(define (descending_sort list)
  (traverse(insert_list_descend_tree '() list))
)

(define (sort_tree func list)
  (func list)
)

(define test_list
  (list 3 6 1 7)
)