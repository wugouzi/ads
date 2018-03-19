(require racket/trace)

(define tree-list
  (call-with-input-file "bst-test.txt"
    (lambda (in)
      (let proc ((x (read in)))
        (if (eof-object? x)
            '()
            (cons x (proc (read in))))))))

(define nil '())

(define (key tree)
  (car tree))

(define (left tree)
  (cadr tree))

(define (right tree)
  (caddr tree))

(define (left-rotate tree)
  (list (key (right tree))
        (list (key tree)
              (left tree)
              (left (right tree)))
        (right (right tree))))

(define (right-rotate tree)
  (list (key (left tree))
        (left (left tree))
        (list (key tree)
              (right (left tree))
              (right tree))))

(define (make-leaf key)
  (list key nil nil))

(define (insert tree new-key)
  (if (null? tree)
      (make-leaf new-key)
      (let ((tree-key (key tree)))
        ;;Whether the new-key is in the node, left subtree or right subtree
        (cond ((= tree-key new-key) tree)
              ((< new-key tree-key)
               (list tree-key
                     (insert (left tree) new-key)
                     (right tree)))
              ((> new-key tree-key)
               (list tree-key
                     (left tree)
                     (insert (right tree) new-key)))))))

;;Find the left-most key of the tree, which is the minimum key
(define (find-left-most-key tree)
  (cond ((null? tree) nil)
        ((null? (left tree)) (key tree))
        (else
         (find-left-most-key (left tree)))))

;;Delete the left-most node
(define (delete-left-most tree)
  (cond ((null? tree) tree)
        ((null? (left tree))
         (if (null? (right tree))
             nil
             (right tree)))
        (else (list (key tree)
                    (delete-left-most tree)
                    (right tree)))))

(define (delete tree delete-key)
  (if (null? tree)
      tree
      (let ((tree-key (key tree)))
        ;;The key lies in left subtree
        (cond ((< delete-key tree-key)
               (list tree-key
                     (delete (left tree) delete-key)
                     (right tree)))
              ;;The key lies in right subtree
              ((> delete-key tree-key)
               (list tree-key
                     (left tree)
                     (delete (right tree) delete-key)))
              ;;Find the key
              ((= delete-key tree-key)
               (cond ((null? (left tree))
                      (right tree))
                     ((null? (right tree))
                      (left tree))
                     (else
                      (list (find-left-most-key (right tree))
                            (left tree)
                            (delete-left-most (right tree))))))))))

(define (insert-list tree keys)
  (if (null? keys)
      tree
      (insert-list (insert tree (car keys)) (cdr keys))))

;;Search operation
(define (search tree search-key)
  (cond ((null? tree) tree)
        ((< search-key (key tree)) (search (left tree) search-key))
        ((> search-key (key tree)) (search (right tree) search-key))
        ((= search-key (key tree)) tree)))

(define (read-tree-list tree-list tree)
  (cond ((null? tree-list) tree)
        ((= 0 (car tree-list))
         (read-tree-list (cddr tree-list)
                         (insert tree (cadr tree-list))))
        ((= 1 (car tree-list))
         (read-tree-list (cddr tree-list)
                         (delete tree (cadr tree-list))))
        ((= 2 (car tree-list))
         (begin
           (search tree (cadr tree-list))
           (read-tree-list (cddr tree-list))))))

(print (read-tree-list tree-list nil))

