"problem 1" 
(define (make-tree value left right)
  (list value left right))
(define (value tree) (car tree))
(define (left tree) (cadr tree))
(define (right tree) (caddr tree))
(define empty-tree? null?)
(define example
(make-tree
'+
(make-tree
'* (make-tree 2 '() '()) (make-tree 3 '() '()))
(make-tree
'*
(make-tree
'+
(make-tree 4 '() '())
(make-tree '- '() ( make-tree 5 '() '())))
(make-tree '1/ '() ( make-tree 6 '() '())))))

(define (apt-value T)
  (cond ((eq? (value T) '+)
         (+ (apt-value (left T)) (apt-value (right T))))
        ((eq? (value T) '-)
         (* -1 (apt-value (right T))))
        ((eq? (value T)  '*)
         (* (apt-value (left T)) (apt-value (right T))))
        ((eq? (value T) '1/)
         (/ 1 (apt-value (right T))))
            (else (value T)))
                   
            )
      
(define example
  (make-tree
   '+
   (make-tree
    '*
    (make-tree 2 '() '())
    (make-tree 3 '() '()))
   (make-tree
    '*
    (make-tree
     '+
(make-tree 4 '() '())
(make-tree '- '() (make-tree 5 '() '())))
(make-tree '1/ '() (make-tree 6 '() '())))))
(apt-value example)
(apt-value (make-tree 5 '() '()))
(apt-value (make-tree '* (make-tree 5 '() '()) (make-tree 44 '() '())))

"problem 2a"
(define (prefix T)
(if (empty-tree? T)
    '()
   (append (list (value T)) (append (prefix (left T)) (prefix (right T))))
   )
  )
(prefix example)

"problem 2b"
(define (postfix T)
  (if (empty-tree? T)
      '()
      (append
       (append (postfix (left T)) (postfix (right T)))
       (list (value T)))
      )
  )
(postfix example)
"help from ps6"
(define (deep-reverse l)
  (cond ((null? l) '())
        ((pair? (car l))
         (append (deep-reverse (cdr l))
                 (list (deep-reverse (car l))))
         )
        (else
         (append (deep-reverse (cdr l))
                 (list (car l)))
         )
        )
  )

"problem 2c"
(define (infixhelp T)
  (if (empty-tree? (value T))
      '()
      (cond ((eq? (value T) '+)
             (list (infixhelp (right T)) '+ (infixhelp (left T))))
            ((eq? (value T) '-)
             (list  (infixhelp (right T)) '-))
            ((eq? (value T) '*)
             (list (infixhelp (right T)) '* (infixhelp (left T))))
            ((eq? (value T) '1/)
             (list (infixhelp (right T)) '1/))
            (else (value T))
            )))
(define (infix T)
  (deep-reverse (infixhelp T))
  )
(infix example)

"problem 3a"
(define (symbol>? a b) 
   (string>? (symbol->string a)(symbol->string b)))
(define (symbol<? a b) 
   (string<? (symbol->string a)(symbol->string b)))

(define (bst-element? item bs-tree)
  (if (empty-tree? bs-tree)
      #f
     (if (equal? item (value bs-tree))
         #t
           (if (symbol<? item (value bs-tree))
               (bst-element? item (left bs-tree))
               (bst-element? item (right bs-tree))
          ))
      )
  )
(bst-element? 'fred (make-tree 'fred '() '()))
(bst-element? 'fred (make-tree 'froggy '() '()))

"problem 3b"
(define (bst-insert item bs-tree)
  (if (bst-element? item bs-tree)
      bs-treez
      (if (empty-tree? bs-tree)
          (make-tree item '() '())
          (if (symbol<? item (value bs-tree))
              (make-tree (value-bs-tree)
                         (bst-insert item (left bs-tree))
                         (right bs-tree))
              (make-tree (value bs-tree)
                         (left bs-tree)
                         (bst-insert item (right bs-tree)))
              )
          )
      )
  )
                         
(bst-insert 'fred '())
(bst-insert 'fred (make-tree 'barney '() '()))

"problem 3c"
(define (bst-smallest bs-tree)
  (if (empty-tree? bs-tree)
      'undefined
      (if (empty-tree? (left bs-tree))
          (value bs-tree)
          (bst-smallest (left bs-tree))
          )
      )
  )
(bst-smallest (make-tree 'gus '() '()))
(bst-smallest (make-tree 'gus
                         (make-tree 'abner '() '())
                         (make-tree 'hari '() '())))

"problem 3d"
(define (bst-largest bs-tree)
  (if (empty-tree? bs-tree)
      'undefined
      (if (empty-tree? (right bs-tree))
          (value bs-tree)
          (bst-largest (right bs-tree))
          )
      )
  )
(bst-largest (make-tree 'gus '() '()))
(bst-largest (make-tree 'gus (make-tree 'abner '() '())(make-tree 'hari '() '()))) 

"problem 3e"
(define (bst-equal? tree1 tree2)
  (if (and (empty-tree? tree1) (empty-tree? tree2))
      #t
      (if (eq? (value tree1) (value tree2))
          (and (bst-equal? (left tree1) (left tree2))
               (bst-equal? (right tree1) (right tree2))
               )
          #f
          )
      )
  )
(bst-equal? (make-tree 'gus '() '()) (make-tree 'gus '() '()))
(bst-equal? (make-tree 'gus '() '()) (make-tree 'gussie '() '()))

"problem 3f"
(define (bst-superset? bst1 bst2)
  (if (empty-tree? bst2)
      #t
      (if (bst-element? (value bst2) bst1)
          (and (bst-superset? bst1 (left bst2))
               (bst-superset? bst1 (right bst2)))
          #f
          )
      )
  )
(bst-superset? (make-tree 'gus '() '()) '())
(bst-superset? '() (make-tree 'gus '() '()) )

"problem 3g"
(define (bst-set-equal? bst1 bst2)
  (if (and (bst-superset? bst1 bst2) (bst-superset? bst2 bst1))
      #t
      #f
      )
  )
(bst-set-equal? (make-tree 'gus '() '()) '())
(bst-set-equal? (make-tree 'gus '() '()) (make-tree 'gus '() '()) ) 

"problem 4a"
(define (bst-delete-min T)
  (if (and (empty-tree? (left T)) (empty-tree? (right T)))
      '()
  (make-tree (value T) (bst-delete-min (left T)) (right T))
  )
  )

(bst-delete-min (make-tree 'fred '() '()))
(bst-delete-min (make-tree 'fred
                           (make-tree 'barney '() '())
                           (make-tree 'joe '() '()))) 

"problem 4b"
(define (bst-delete-max T)
  (if (and (empty-tree? (left T)) (empty-tree? (right T)))
      '()
      (make-tree (value T) (left T) (bst-delete-max (right T)))
      )
  )
(bst-delete-max (make-tree 'fred '() '()))
(bst-delete-max (make-tree 'fred
                           (make-tree 'barney '() '())
                           (make-tree 'joe '() '())))

"problem 4c"
(define (bst-delete val bst)
  (cond
    ((not (bst-element? val bst))
     bst)
        ((empty-tree? bst) '())
        ((symbol>? val (value bst))
         (make-tree (value bst)
                    (left bst)
                     (bst-delete val (right bst))))
        ((symbol<? val (value bst))
         (make-tree (value bst)
                    (bst-delete val (left bst))
                    (right bst)))                          
        (else
         (cond
           ((and (empty-tree? (left bst)) (empty-tree? (right bst)))
               '())
           ((and (empty-tree? (right bst)) (not (empty-tree? (left bst))))
            (left bst))
           ((and (empty-tree? (left bst)) (not (empty-tree? (right bst))))
            (right bst))
        (else (bst-largest (left bst)))
        )
         )
        )
  )
  

        
 
                 
(bst-delete 'fred (make-tree 'fred '() '())) 

(bst-delete 'jack (make-tree 'fred '() '()))
