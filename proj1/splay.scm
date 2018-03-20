;#lang racket
;;tree (key left-subtree right-subtree)
(require racket/trace)
;;We use a top-down version implement which is recommended by the
;;original paper and have a slightly different result with the
;;bottom-up method.
;;We begin the splay with two help trees, the left and the right.
;;(1) Node y contains the access item
;;      +-+       (x)      +-+                  +-+     (y)      +-+                
;;      +-+       /  \     +-+  ----------->    +-+     /A\      +/+ R
;;       L      (y)  /B\    R                    L               /
;;              /A\                                            (x)
;;                                                               \
;;                                                              /B\
;;
;;(2) Zig-zig
;;      +-+       (x)      +-+                  +-+     (z)      +-+                
;;      +-+       /  \     +-+  ----------->    +-+     /A\      +/+ R
;;       L      (y)  /C\    R                    L               /
;;              / \                                            (y)
;;            (z) /B\                                             \
;;            /A\                                                 (x)
;;                                                               /   \
;;                                                             /B\  /C\
;;
;;(3) Zig-zag
;;      +-+       (x)      +-+                  +-+     (z)      +-+                
;;      +-+       /  \     +-+  ----------->  L +\+     /A\      +/+ R
;;       L      (y)  /C\    R                     \              /
;;              / \                               (y)          (x)
;;            /A\ (z)                             /              \
;;                /B\                           /A\              /C\
;;
;;(4) Merge three trees and x contains the accessed item
;;      +-+       (x)      +-+                          (x)
;;      +-+       /  \     +-+  ----------->            / \
;;       L      /A\ /B\     R                          /   \
;;                                                    /     \
;;                                                  +-+     +-+
;;                                                L +\+     +/+ R
;;                                                    \     /                
;;                                                   /A\   /B\
;;

;;Read input and make them a list
(define tree-list
  (call-with-input-file "splay-test.txt"
    (lambda (in)
      (let proc ((x (read in)))
        (if (eof-object? x)
            '()
            (cons x (proc (read in))))))))

;(define out (open-output-file "splay-result.txt"))

(define nil '())

;;The Key of tree
(define (key tree)
  (car tree))

;;Left subtree of tree
(define (left tree)
  (cadr tree))

;;Right subtree of tree
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

;;Insert add-tree to be the right leaf of the right-most nodes of the tree
(define (add-to-right-most tree add-tree)
  (cond ((null? tree) add-tree)
        ((null? add-tree) tree)
        (else (if (null? (right tree))
                  (list (key tree) (left tree) add-tree)
                  (list (key tree) (left tree) (add-to-right-most (right tree) add-tree))))))

;;Leftpp
(define (add-to-left-most tree add-tree)
  (cond ((null? tree) add-tree)
        ((null? add-tree) tree)
        (else (if (null? (left tree))
                  (list (key tree) add-tree (right tree))
                  (list (key tree) (add-to-left-most (left tree) add-tree) (right tree))))))

;;The right-most node of the tree
(define (right-most tree)
  (if (null? (right tree))
      tree
      (right-most (right tree))))


(define (merge-tree main-tree left-tree right-tree)
  (if (null? main-tree)
      (cond ((null? left-tree) right-tree)
            ((null? right-tree) left-tree)
            (else
             (add-to-left-most right-tree left-tree)))
      (list (key main-tree)
        (add-to-right-most left-tree (left main-tree))
        (add-to-left-most right-tree (right main-tree)))))

(define (make-splay-right-tree tree)
  (list (key tree) nil (right tree)))

(define (make-splay-left-tree tree)
  (list (key tree) (left tree) nil))

(define (top-down-splay tree search-key left-tree right-tree)
  (if (null? tree)
      ;;Doesn't have the expected key and merge tree
      (merge-tree tree left-tree right-tree)
      (let ((tree-key (key tree)))
       (cond ((and (null? (left tree))
                   (null? (right tree))) (merge-tree tree left-tree right-tree))
             ;;Case 4
             ((= search-key tree-key) (merge-tree tree left-tree right-tree))
             ;;search-key is in left subtree
             ((< search-key tree-key)
              (cond ((null? (left tree))
                     (merge-tree tree left-tree right-tree))
                    (else
                     (let ((left-key (key (left tree))))
                       (cond
                        ;;Case 2
                        ((< search-key left-key)
                         (let ((newtree (right-rotate tree)))
                           (top-down-splay (left newtree)
                                           search-key
                                           left-tree
                                           (add-to-left-most right-tree
                                                             (make-splay-right-tree newtree)))))
                        ;;Case 3
                        ((> search-key left-key)
                         (top-down-splay (right (left tree))
                                         search-key
                                         (add-to-right-most left-tree
                                                            (make-splay-left-tree (left tree)))
                                         (add-to-left-most right-tree
                                                           (make-splay-right-tree tree))))
                        ;;Case 1
                        ((= search-key left-key)
                         (merge-tree (left tree)
                                     left-tree
                                     (add-to-left-most right-tree
                                                       (make-splay-right-tree tree)))))))))
             ((> search-key tree-key)
              (if (null? (right tree))
                  (merge-tree tree left-tree right-tree)
                  (let ((right-key (key (right tree))))
                    (cond
                     ;;Case 2
                     ((> search-key right-key)
                      (let ((newtree (left-rotate tree)))
                        (top-down-splay (right newtree)
                                        search-key
                                        (add-to-right-most left-tree
                                                           (make-splay-left-tree newtree))
                                        right-tree)))
                     ;;Case 3
                     ((< search-key right-key)
                      (top-down-splay (left (right tree))
                                      search-key
                                      (add-to-right-most left-tree
                                                         (make-splay-left-tree tree))
                                      (add-to-left-most right-tree
                                                        (make-splay-right-tree (right tree)))))
                     ;;Case 1
                     ((= search-key right-key)
                      (merge-tree (right tree)
                                  (add-to-right-most left-tree
                                                     (make-splay-left-tree tree))
                                  right-tree))))))))))

;;Insert
(define (insert-splay tree new-key)
  ;;First insert key like binary search tree and then splay it
  (define (insert tree new-key)
    (cond
     ((null? tree)
      (list new-key nil nil))
     ;;The key is in left subtree
     ((< new-key (key tree))
      (if (null? (left tree))
          (list (key tree)
                (list new-key nil nil)
                (right tree))
          (list (key tree)
                (insert (left tree) new-key)
                (right tree))))
     ;;The key is in right subtree
     ((> new-key (key tree))
      (if (null? (right tree))
          (list (key tree)
                (left tree)
                (list new-key nil nil))
          (list (key tree)
                (left tree)
                (insert (right tree) new-key))))))
  (top-down-splay (insert tree new-key) new-key nil nil))

(define (delete tree delete-key)
  ;;First splay the tree
  (let ((splaytree (top-down-splay tree delete-key nil nil)))
    ;;Whether there is the expected key
    (cond ((not (= delete-key (key splaytree))) splaytree)
          ((null? (left splaytree)) (right splaytree))
          (else
           ;;Merge the left and right tree
           (let ((newtree (top-down-splay (left splaytree)
                                          (key (right-most (left splaytree)))
                                          nil nil)))
             (list (key newtree)
                   (left newtree)
                   (right splaytree)))))))

(define (search tree search-key)
  (top-down-splay tree search-key nil nil))

(define (insert-list tree keys)
  (if (null? keys)
      tree
      (insert-list (insert-splay tree (car keys)) (cdr keys))))

;;Read the input list
(define (read-tree-list tree-list tree)
  (cond ((null? tree-list) tree)
        ((= 0 (car tree-list))
         (read-tree-list (cddr tree-list)
                         (insert-splay tree (cadr tree-list))))
        ((= 1 (car tree-list))
         (read-tree-list (cddr tree-list)
                         (delete tree (cadr tree-list))))
        ((= 2 (car tree-list))
         (begin
           (search tree (cadr tree-list))
           (read-tree-list (cddr tree-list))))))

(display (read-tree-list tree-list nil))
