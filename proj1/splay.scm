;;tree (key left-subtree right-subtree)
(require racket/trace)
(define nil '())

(define (key tree)
  (car tree))

(define (left tree)
  (cadr tree))

(define (right tree)
  (caddr tree))

(define (right-rotate tree)
  (list (key (left tree))
        (left (left tree))
        (list (key tree) (right (left tree)) (right tree))))

(define (left-rotate tree)
  (list (key (right tree))
        (list (key tree) (left tree) (left (right tree)))
        (right (right tree))))

(define (right-right-rotate tree)
  (right-rotate (right-rotate tree)))

(define (left-left-rotate tree)
  (left-rotate (left-rotate tree)))

(define (left-right-rotate tree)
  (right-rotate (list (key tree)
                      (left-rotate (left tree))
                      (right tree))))

(define (right-left-rotate tree)
  (left-rotate (list (key tree)
                     (left tree)
                     (right-rotate (right tree)))))

(define test-tree '(5 (1 () (4 () () 1) 2) (8 (7 () () 1) (23 () () 1) 2) 3))

(define (add-to-right-most tree add-tree)
  (cond ((null? tree) add-tree)
        ((null? add-tree) tree)
        (else (if (null? (right tree))
                  (list (key tree) (left tree) add-tree)
                  (list (key tree) (left tree) (add-to-right-most (right tree) add-tree))))))

(define (add-to-left-most tree add-tree)
  (cond ((null? tree) add-tree)
        ((null? add-tree) tree)
        (else (if (null? (left tree))
                  (list (key tree) add-tree (right tree))
                  (list (key tree) (add-to-left-most (left tree) add-tree) (right tree))))))

(define (merge-tree main-tree left-tree right-tree)
  (list (key main-tree)
        (add-to-right-most left-tree (left main-tree))
        (add-to-left-most right-tree (right main-tree))))

(define (make-splay-right-tree tree)
  (list (key tree) nil (right tree)))

(define (make-splay-left-tree tree)
  (list (key tree) (left tree) nil))

(define (top-down-splay tree search-key left-tree right-tree)
  (let ((tree-key (key tree)))
    (cond ((and (null? (left tree))
                (null? (right tree))) (merge-tree tree left-tree right-tree))
          ((= search-key tree-key) (merge-tree tree left-tree right-tree))
          ((< search-key tree-key)
           (cond ((null? (left tree))
                  (merge-tree tree left-tree right-tree))
                 (else
                  (let ((left-key (key (left tree))))
                    (cond ((< search-key left-key)
                           (let ((newtree (right-rotate tree)))
                             (top-down-splay (left newtree)
                                             search-key
                                             left-tree
                                             (add-to-left-most right-tree
                                                               (make-splay-right-tree newtree)))))
                          ((> search-key left-key)
                           (top-down-splay (right (left tree))
                                           search-key
                                           (add-to-right-most left-tree
                                                              (make-splay-left-tree (left tree)))
                                           (add-to-left-most right-tree
                                                             (make-splay-right-tree tree))))
                          ((= search-key left-key)
                           (merge-tree (left tree)
                                       left-tree
                                       (add-to-left-most right-tree
                                                         (make-splay-right-tree tree)))))))))
          ((> search-key tree-key)
           (if (null? (right tree))
               (merge-tree tree left-tree right-tree)
               (let ((right-key (key (right tree))))
                 (cond ((> search-key right-key)
                        (let ((newtree (left-rotate tree)))
                          (top-down-splay (right newtree)
                                          search-key
                                          (add-to-right-most left-tree
                                                             (make-splay-left-tree newtree))
                                          right-tree)))
                       ((< search-key right-key)
                        (top-down-splay (left (right tree))
                                        search-key
                                        (add-to-right-most left-tree
                                                           (make-splay-left-tree tree))
                                        (add-to-left-most right-tree
                                                          (make-splay-right-tree (right tree)))))
                       ((= search-key right-key)
                        (merge-tree (right tree)
                                    (add-to-right-most left-tree
                                                       (make-splay-left-tree tree))
                                    right-tree)))))))))

(define (insert-splay tree new-key)
  (define (insert tree new-key)
    (cond ((null? tree)
           (list new-key nil nil))
          ((< new-key (key tree))
           (if (null? (left tree))
               (list (key tree)
                     (list new-key nil nil)
                     (right tree))
               (list (key tree)
                     (insert (left tree) new-key)
                     (right tree))))
          ((> new-key (key tree))
           (if (null? (right tree))
               (list (key tree)
                     (left tree)
                     (list new-key nil nil))
               (list (key tree)
                     (left tree)
                     (insert (right tree) new-key))))))
  (top-down-splay (insert tree new-key) new-key nil nil))

(define (right-most tree)
  (if (null? (right tree))
      tree
      (right-most (right tree))))

(define (delete tree delete-key)
  
  (let ((splaytree (splay tree delete-key)))
    (if (null? (left splaytree))
        (right splaytree)
        (let ((newtree (splay splaytree (key (right-most (left splaytree))))))
          (list (key newtree)
                (left newtree)
                (right splaytree))))))

(define (search tree search-key)
  (splay tree search-key))

(define (insert-list tree keys)
  (if (null? keys)
      tree
      (insert-list (insert-splay tree (car keys)) (cdr keys))))



;(insert-list nil '(3 1 4 7 8 2 5 6 0))
(define t-tree '(0 () (5 (1 () (2 () (4 (3 () ()) ()))) (6 () (7 () (8 () ()))))))
;(insert-list nil '(3 1 4 7 8 2 5 6 0 29))
;'(29 (5 (0 () (1 () (2 () (4 (3 () ()) ())))) (7 (6 () ()) (8 () ()))) ())

