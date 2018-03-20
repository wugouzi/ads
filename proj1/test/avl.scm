;#!/usr/bin/racket
;#lang racket
;;tree:: (key left-subtree right-subtree height)
(require racket/trace)

(define tree-list
  (call-with-input-file "testcase.txt"
    (lambda (in)
      (let proc ((x (read in)))
        (if (eof-object? x)
            '()
            (cons x (proc (read in))))))))

(define nil '())

;;Key of the tree
(define (key tree)
  (car tree))

;;Left subtree
(define (left tree)
  (cadr tree))

;;Right subtree
(define (right tree)
  (caddr tree))

;;Height of the tree, we assume leaf has height 1
(define (height tree)
  (if (null? tree)
      0
      (cadddr tree)))

;;Balance factor of the tree
(define (factor tree)
  (- (height (left tree))
     (height (right tree))))

;;Make a leaf
(define (make-leaf key)
  (list key nil nil 1))


;;Make an avl-tree
(define (make-avl-tree key left right)
  (list key left right
        (+ 1 (max (height left)
                  (height right)))))

(define (left-rotate tree)
  (if (null? tree)
      tree
      (make-avl-tree (key (right tree))
                     (make-avl-tree (key tree) (left tree) (left (right tree)))
                     (right (right tree)))))

(define (right-rotate tree)
  (if (null? tree)
      tree
      (make-avl-tree (key (left tree))
                     (left (left tree))
                     (make-avl-tree (key tree) (right (left tree)) (right tree)))))

(define (right-left-rotate tree)
  (if (null? tree)
      tree
      (make-avl-tree (key (left (right tree)))
                     (make-avl-tree (key tree)
                                    (left tree)
                                    (left (left (right tree))))
                     (make-avl-tree (key (right tree))
                                    (right (left (right tree)))
                                    (right (right tree))))))

(define (left-right-rotate tree)
  (if (null? tree)
      tree
      (make-avl-tree (key (right (left tree)))
                     (make-avl-tree (key (left tree))
                                    (left (left tree))
                                    (left (right (left tree))))
                     (make-avl-tree (key tree)
                                    (right (right (left tree)))
                                    (right tree)))))

;;Balance the tree
(define (avl-balance tree)
  (let ((avl-factor (factor tree)))
    (cond
     ;;The balance factor is 2
     ((= 2 avl-factor)
      (if (> 0 (factor (left tree)))
          ;;Left right case since the bf of
          ;;the left tree is larger than 1
          (left-right-rotate tree)
          ;;Left left case
          (right-rotate tree)))
     ((= -2 avl-factor)
      (if (> (factor (right tree)) 0)
          ;;Right left case
          (right-left-rotate tree)
          ;;Right right case
          (left-rotate tree)))
     ;;The tree doesn't need balance
     (else tree))))

(define (insert-avl tree new-key)
  (cond ((null? tree) (make-leaf new-key))
        ((< new-key (key tree))
         (if (null? (left tree))
             (make-avl-tree (key tree) (make-leaf new-key) (right tree))
             (let ((newtree (make-avl-tree (key tree)
                                           (insert-avl (left tree) new-key)
                                           (right tree))))
               (avl-balance newtree))))
        ((> new-key (key tree))
         (if (null? (right tree))
             (make-avl-tree (key tree) (left tree) (make-leaf new-key))
             (let ((newtree (make-avl-tree (key tree)
                                           (left tree)
                                           (insert-avl (right tree) new-key))))
               (avl-balance newtree))))
        (else tree)))


(define (delete tree delete-key)
  ;;Find the left-most node of the tree
  ;;which is the inorder successor of the delete-key
  (define (left-most tree)
    (if (null? (left tree))
        tree
        (left-most (left tree))))
  ;;Delete the left-most nodes of the tree
  (define (delete-left-most tree)
    (cond ((null? (left tree))
           (right tree))
          (else
           (make-avl-tree (key tree)
                          (delete-left-most (left tree))
                          (right tree)))))
  (let ((tree-key (key tree)))
    (cond
     ;;No such key
     ((null? tree) tree)
     ;;The key is in left subtree
     ((< delete-key tree-key)
      (if (null? (left tree))
          tree
          (let ((newtree (make-avl-tree tree-key
                                        (delete (left tree) delete-key)
                                        (right tree))))
            ;;Balance the tree. Since we use a recursive way to delete
            ;;tree, thus we only balance one node in practice.
            (avl-balance newtree))))
     ;;The key is in right subtree
     ((> delete-key tree-key)
      (if (null? (right tree))
          tree
          (let ((newtree (make-avl-tree tree-key
                                        (left tree)
                                        (delete (right tree) delete-key))))
            (avl-balance newtree))))
     ;;Find the key
     ((= delete-key tree-key)
      (cond ((null? (left tree)) (right tree))
                          ((null? (right tree)) (left tree))
                          ;;The nodes have both right subtree and left subtree
                          ;;So we need to find its inorder successor and take
                          ;;the place of it
                          (else (make-avl-tree (key (left-most (right tree)))
                                               (left tree)
                                 (delete-left-most (right tree)))))))))

;;Insert a list of keys into trees
(define (insert-list tree keys)
  (if (null? keys)
      tree
      (insert-list (insert-avl tree (car keys)) (cdr keys))))

(define (search tree search-key)
  (cond ((null? tree) tree)
        ((< search-key (key tree)) (search (left tree) search-key))
        ((> search-key (key tree)) (search (right tree) search-key))
        ((= search-key (key tree)) tree)))

;;Simple test tree
(define test-tree '(5 (1 () (4 () () 1) 2) (8 (7 () () 1) (23 () () 1) 2) 3))

;;Read from the tree-list
;;0 stands for insert, 1 is delete and 2 is search
(define (read-tree-list tree-list tree)
  (cond ((null? tree-list) tree)
        ((= 0 (car tree-list))
         (read-tree-list (cddr tree-list)
                         (insert-avl tree (cadr tree-list))))
        ((= 1 (car tree-list))
         (read-tree-list (cddr tree-list)
                         (delete tree (cadr tree-list))))
        ((= 2 (car tree-list))
         (begin
           (search tree (cadr tree-list))
           (read-tree-list (cddr tree-list))))))


;;edited by the tester, intended to calculate the time it cost
(define (time_test start_time)
  (print (read-tree-list tree-list nil))
  (print (list (newline) "total run time: " (- (current-inexact-milliseconds) start_time) "milliseconds!")))

;;Output the tree and the time
(time_test (current-inexact-milliseconds))
