#lang racket

(provide (all-defined-out))

(define integers-from
  (lambda (n)
    (cons-lzl n (lambda () (integers-from (+ n 1))))))

(define cons-lzl cons)
(define empty-lzl? empty?)
(define empty-lzl '())
(define head car)
(define tail
  (lambda (lzl)
    ((cdr lzl))))


(define make-tree list)
(define add-subtree cons)
(define make-leaf (lambda (x) x))
(define empty-tree? empty?)
(define first-subtree car)
(define rest-subtree cdr)
(define leaf-data (lambda (x) x))
(define composite-tree? list?)
(define leaf? (lambda (x) (not (composite-tree? x))))
(define id (lambda (x) x))
(define empty-tree '())

;; Signature: map-lzl(f, lz)
;; Type: [[T1 -> T2] * Lzl(T1) -> Lzl(T2)]
(define map-lzl
  (lambda (f lzl)
    (if (empty-lzl? lzl)
        lzl
        (cons-lzl (f (head lzl))
                  (lambda () (map-lzl f (tail lzl)))))))

;; Signature: take(lz-lst,n)
;; Type: [LzL*Number -> List]
;; If n > length(lz-lst) then the result is lz-lst as a List
(define take
  (lambda (lz-lst n)
    (if (or (= n 0) (empty-lzl? lz-lst))
      empty-lzl
      (cons (head lz-lst)
            (take (tail lz-lst) (- n 1))))))

; Signature: nth(lz-lst,n)
;; Type: [LzL*Number -> T]
;; Pre-condition: n < length(lz-lst)
(define nth
  (lambda (lz-lst n)
    (if (= n 0)
        (head lz-lst)
        (nth (tail lz-lst) (- n 1)))))

;;; Q1.1
; Signature: append$(lst1, lst2, cont) 
; Type: [List * List * [List -> T]] -> T
; Purpose: Returns the concatination of the given two lists, with cont pre-processing
(define append$
 (lambda (lst1 lst2 cont)
   (if (empty? lst1)
       (cont lst2)
       (append$ (cdr lst1) lst2 (lambda (append-1) (cont (cons (car lst1) append-1)))))))

;;; Q1.2
; Signature: equal-trees$(tree1, tree2, succ, fail) 
; Type: [Tree * Tree * [Tree ->T1] * [Pair->T2] -> T1 U T2
; Purpose: Determines the structure identity of a given two lists, with post-processing succ/fail

;(define leaf? (lambda (x) (not (list? x))))  @ mistake in the files two leaf?

(define equal-trees$ 
 (lambda (tree1 tree2 succ fail)
   (cond ((leaf? tree1)
          (if (leaf? tree2)
              (succ (make-leaf (cons (leaf-data tree1) (leaf-data tree2))))
              (fail (cons tree1 tree2))))
         ((leaf? tree2)
          (if (leaf? tree1)
              (succ (make-leaf (cons (leaf-data tree1) (leaf-data tree2))))
              (fail (cons tree1 tree2))))
         ((empty-tree? tree1)
          (if (empty-tree? tree2)
              (succ empty-tree)
              (fail (cons tree1 tree2))))
         ((empty-tree? tree2)
          (if (empty-tree? tree1)
              (succ empty-tree)
              (fail (cons tree1 tree2))))
         (else ;compsite trees
          (equal-trees$ (first-subtree tree1)
                        (first-subtree tree2)
                        (lambda (first-res) ;succ
                          (equal-trees$ (rest-subtree tree1)
                                        (rest-subtree tree2)
                                        (lambda (rest-res) ;succ
                                          (succ (add-subtree first-res rest-res)))
                                        (lambda (rest-pair-fail) ;fail
                                          (fail rest-pair-fail)
                                         )))
                        (lambda (first-pair-fail) ;fail
                          (fail first-pair-fail)))))
 )
)

;;; Q2a
; Signature: reduce1-lzl(reducer, init, lzl) 
; Type: [T2*T1 -> T2] * T2 * LzL<T1> -> T2
; Purpose: Returns the reduced value of the given lazy list
(define reduce1-lzl 
  (lambda (reducer init lzl)
   (if (empty-lzl? lzl)
       init
       (reduce1-lzl reducer (reducer init (head lzl)) (tail lzl)))
  )
)  

;;; Q2b
; Signature: reduce2-lzl(reducer, init, lzl, n) 
; Type: [T2*T1 -> T2] * T2 * LzL<T1> * Number -> T2
; Purpose: Returns the reduced value of the first n items in the given lazy list
;@TODO can we use or?
(define reduce2-lzl 
  (lambda (reducer init lzl n)
    (if (or (empty-lzl? lzl) (= n 0))
       init
       (reduce2-lzl reducer (reducer init (head lzl)) (tail lzl) (- n 1)))
  )
)  

;;; Q2c
; Signature: reduce3-lzl(reducer, init, lzl) 
; Type: [T2 * T1 -> T2] * T2 * LzL<T1> -> Lzl<T2>
; Purpose: Returns the reduced values of the given lazy list items as a lazy list
(define reduce3-lzl 
  (lambda (reducer init lzl)
   (if (empty-lzl? lzl)
       lzl
       (cons-lzl (reducer init (head lzl)) (lambda () (reduce3-lzl reducer (reducer init (head lzl)) (tail lzl)))))
  )
)  
 
;;; Q2e
; Signature: integers-steps-from(from,step) 
; Type: Number * Number -> Lzl<Number>
; Purpose: Returns a list of integers from 'from' with 'steps' jumps
(define integers-steps-from
  (lambda (from step)
    (cons-lzl from (lambda () (integers-steps-from (+ from step) step)))
  )
)

;;; Q2f
; Signature: generate-pi-approximations() 
; Type: Empty -> Lzl<Number>
; Purpose: Returns the approximations of pi as a lazy list
(define generate-pi-approximations
  (lambda ()
    (let
        ((lzl1 (integers-steps-from 1 4))
         (lzl2 (integers-steps-from 3 4)))
      (let
          ((lzl-prod (mult-lzls lzl1 lzl2)))
        (let
            ((lzl-inverse (map-lzl inverse lzl-prod)))
          (let
              ((lzl-final (map-lzl mult8 lzl-inverse)))
          (reduce3-lzl + 0 lzl-final)))))))


;;Auxliary function
; Signature: mult-lzls(lzl1, lzl2)
; Type: Lzl<Number> * Lzl<Number> -> Lzl<Number>
; Purpose: Returns the lzl list of the elementwise multipication
(define mult-lzls
  (lambda (lzl1 lzl2)
    (cons-lzl (* (head lzl1) (head lzl2)) (lambda () (mult-lzls (tail lzl1) (tail lzl2))))
  )
)

(define inverse
  (lambda (z) (/ 1 z)))

(define mult8
  (lambda (x) (* 8 x)))



  



