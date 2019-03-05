#lang racket
(require 2htdp/batch-io)
(require "decision_functions.rkt")

;input dataset
(provide toytrain)
(define toytrain "../data/toy_train.csv")

(define (read-file file-path)
  (call-with-input-file file-path
    (lambda (fin) (port->string fin))))

(provide titanictrain)
(define titanictrain "../data/titanic_train.csv")

(provide mushroomtrain)
(define mushroomtrain "../data/mushrooms_train.csv")

;output tree (dot file)
(provide toyout)
(define toyout "../output/toy-decision-tree.dot")

;reading input datasets
;read the csv file myfile as a list of strings
;with each line of the original file as an element of the list
;further split each line at commas
;so then we have a list of list of strings
(provide toy-raw)
(define toy-raw '...)

(provide titanic-raw)
(define titanic-raw "../data/titanic_raw.csv")

(provide mushroom-raw)
(define mushroom-raw "../data/mushroom_raw.csv")

;function to convert data to internal numerical format
;(features . result)
(provide format)
(define (format data) '...)

;list of (features . result)
(provide toy)
(define toy '...)

(provide titanic)
(define titanic '...)

(provide mushroom)
(define mushroom '...)

;============================================================================================================
;============================================================================================================
;============================================================================================================

;get fraction of result fields that are 1
;used to find probability value at leaf
(provide get-leaf-prob)
(define (get-leaf-prob data)
  '...
  )

;get entropy of dataset
(provide get-entropy)
(define (get-entropy data)
  '...
  )

;find the difference in entropy achieved
;by applying a decision function f to the data
(provide entropy-diff)
(define (entropy-diff f data)
  '...
  )

;choose the decision function that most reduces entropy of the data
(provide choose-f)
(define (choose-f candidates data) ; returns a decision function
  '...
  )

(provide DTree)
(struct DTree (desc func kids))

;build a decision tree (depth limited) from the candidate decision functions and data
(provide build-tree)
(define (build-tree candidates data depth)
  '...
  )

;given a test data (features only), make a decision according to a decision tree
;returns probability of the test data being classified as 1
(provide make-decision)
(define (make-decision tree test)
 '...
  )

;============================================================================================================
;============================================================================================================
;============================================================================================================

;annotate list with indices
(define (pair-idx lst n)
  (if (empty? lst) `() (cons (cons (car lst) n) (pair-idx (cdr lst) (+ n 1))))
  )

;generate tree edges (parent to child) and recurse to generate sub trees
(define (dot-child children prefix tabs)
  (apply string-append (map (lambda (t) (string-append tabs "r" prefix "--" "r" prefix (~a (cdr t)) "[label=\"" (~a (cdr t)) "\"];" "\n" (dot-helper (car t) (string-append prefix (~a (cdr t))) (string-append tabs "\t")))) children))
  )

;generate tree nodes and call function to generate edges
(define (dot-helper tree prefix tabs)
  (let* ([node (match tree [(DTree d f c) (cons d c)])]
         [f (car node)]
         [c (cdr node)])
    (string-append tabs "r" prefix "[label=\"" f "\"];" "\n\n" (dot-child (pair-idx c 0) prefix tabs))
    )
  )

;output tree (dot file)
(provide display-tree)
(define (display-tree tree dtfile)
  (write-file dtfile (string-append "graph \"decision-tree\" {" "\n" (dot-helper tree "" "\t") "}"))
  )
;============================================================================================================
;============================================================================================================
;============================================================================================================