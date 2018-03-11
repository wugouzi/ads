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

(define (splay tree search-key)
  (let ((tree-key (key tree)))
   (cond ((or (null? tree)
              (= search-key tree-key)
              (and (null? (left tree))
                   (null? (right tree))))
          tree)
         ((and (< search-key tree-key)
               (not (null? (left tree))))
          (let ((left-key (key (left tree))))
            (cond ((< search-key left-key)
                   (let ((newtree (list tree-key
                                        (list left-key
                                              (splay (left (left tree)) search-key)
                                              (right (left tree)))
                                        (right tree))))
                     (if (null? (left (left newtree)))
                         (right-rotate newtree)
                         (right-right-rotate newtree))))
                  ((> search-key left-key)
                   (let ((newtree (list tree-key
                                        (list left-key
                                              (left (left tree))
                                              (splay (right (left tree)) search-key))
                                        (right tree))))
                     (if (null? (right (left newtree)))
                         (right-rotate newtree)
                         (left-right-rotate newtree))))
                  (else
                   (right-rotate tree)))))
         ((and (> search-key tree-key)
               (not (null? (right tree))))
          (let ((right-key (key (right tree))))
            (cond ((> search-key right-key)
                   (let ((newtree (list tree-key
                                        (left tree)
                                        (list right-key
                                              (left (right tree))
                                              (splay (right (right tree)) search-key)))))
                     (if (null? (right (right newtree)))
                         (left-rotate newtree)
                         (left-left-rotate newtree))))
                  ((< search-key right-key)
                   (let ((newtree (list tree-key
                                        (left tree)
                                        (list right-key
                                              (splay (left (right tree)) search-key)
                                              (right (right tree))))))
                     (if (null? (left (right newtree)))
                         (left-rotate newtree)
                         (right-left-rotate newtree))))
                  (else
                   (left-rotate tree)))))
         (else tree))))

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
  (splay (insert tree new-key) new-key))

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


