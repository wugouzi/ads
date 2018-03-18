;#!/usr/bin/racket
;#lang racket

;;tree:: (key left-subtree right-subtree height)
;(require racket/trace)
;(define tr (map string->number (string-split l)))
(define tree-list
  (call-with-input-file "avl-test.txt"
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

(define (height tree)
  (if (null? tree)
      0
      (cadddr tree)))

(define (factor tree)
  (- (height (left tree))
     (height (right tree))))

(define (make-leaf key)
  (list key nil nil 1))

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

(define (avl-balance tree)
  (let ((avl-factor (factor tree)))
    (cond ((= 2 avl-factor)
           (if (> 0 (factor (left tree)))
               (left-right-rotate tree)
               (right-rotate tree)))
          ((= -2 avl-factor)
           (if (> (factor (right tree)) 0)
               (right-left-rotate tree)
               (left-rotate tree)))
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
  (define (left-most tree)
    (if (null? (left tree))
        tree
        (left-most (left tree))))
  (define (delete-left-most tree)
    (cond ((null? (left tree))
           (right tree))
          (else
           (make-avl-tree (key tree)
                          (delete-left-most (left tree))
                          (right tree)))))
  (let ((tree-key (key tree)))
   (cond ((null? tree) tree)
         ((< delete-key tree-key)
          (if (null? (left tree))
              tree
              (let ((newtree (make-avl-tree tree-key
                                            (delete (left tree) delete-key)
                                            (right tree))))
                (avl-balance newtree))))
         ((> delete-key tree-key)
          (if (null? (right tree))
              tree
              (let ((newtree (make-avl-tree tree-key
                                            (left tree)
                                            (delete (right tree) delete-key))))
                (avl-balance newtree))))
         ((= delete-key tree-key)
          (let ((newtree (cond ((null? (left tree)) (right tree))
                               ((null? (right tree)) (left tree))
                               (else (make-avl-tree (key (left-most (right tree)))
                                                    (left tree)
                                                    (delete-left-most (right tree)))))))
            (avl-balance newtree))))))

(define (insert-list tree keys)
  (if (null? keys)
      tree
      (insert-list (insert-avl tree (car keys)) (cdr keys))))

(define (search tree search-key)
  (cond ((null? tree) tree)
        ((< search-key (key tree)) (search (left tree) search-key))
        ((> search-key (key tree)) (search (right tree) search-key))
        ((= search-key (key tree)) tree)))
;(print (car (insert-list nil tr)))

(define test-tree '(5 (1 () (4 () () 1) 2) (8 (7 () () 1) (23 () () 1) 2) 3))

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

(print (read-tree-list tree-list nil))
