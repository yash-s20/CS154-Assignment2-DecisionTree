#lang racket
(require 2htdp/batch-io)
(require "decision_functions.rkt")

;input dataset
(provide toytrain)
(define toytrain "../data/toy_train.csv")


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
(define toy-raw (cdr (read-csv-file toytrain))) ;'("Result"  <- class, attributes ->  "Feature1" "Feature2" "Feature2" "Feature4")

(provide titanic-raw)
(define titanic-raw (cdr (map (lambda(x) (cddr x)) (read-csv-file titanictrain)))) ;'("Survived" <- class, attributes -> "Pclass" "Sex" "Age" "SibSp" "Parch" "Fare" "Embarked")

(provide mushroom-raw)
(define mushroom-raw (cdr (read-csv-file mushroomtrain))) ;'("edible"  <- class, attributes ->  "cap-shape" "Cap-surface" "bruises" "odor" "gill-attachment" "gill-spacing" "gill-size" "stalk-shape" "ring-number" "population" "habitat")

;function to convert data to internal numerical format
;(features . result)
(define (map-string->number lst)
  (map string->number lst))
(provide format)
(define (format data) (map (lambda(x) (let ([y (map-string->number x)]) (cons (cdr y) (car y)))) data))

;list of (features . result)
(provide toy)
(define toy (format toy-raw))

(provide titanic)
(define titanic (format titanic-raw))

(provide mushroom)
(define mushroom (format mushroom-raw))

;============================================================================================================
;============================================================================================================
;============================================================================================================

;get fraction of result fields that are 1
;used to find probability value at leaf
(provide get-leaf-prob)
(define (get-leaf-prob data)
  '...
  )

(define toy-features
  (list y1 y2 y3 y4>62))

(define titanic-features
  (list pclass sex age>25 sibsp parch fare>50 emb))

(define mushroom-features
  (list cshape csurf bruise odor gatch gspace gsize sshape nring pop hab))

;get entropy of dataset
(define (x-logx x)
  (if (= x 0) 0 (* x (log x 2))))
(provide get-entropy)
(define (get-entropy data)
  (let* ([z (/ (count (lambda(x) (zero? (cdr x))) data) (length data))])
    (- (+ (x-logx z) (x-logx (- 1 z))))))

;find the difference in entropy achieved
;by applying a decision function f to the data
(define (list-of-empty-lists k acc)
  (match k
    [0 acc]
    [_ (list-of-empty-lists (- k 1) (cons '() acc))]))


(define (bucketing data buckets f)
    (define (insert-at lst k x)
      (if (= k 0) (cons (cons x (car lst)) (cdr lst)) (cons (car lst) (insert-at (cdr lst) (- k 1) x))))
    (if (null? data) buckets (bucketing (cdr data) (insert-at buckets (f (caar data)) (car data)) f)))

(provide entropy-diff)
(define (entropy-diff f data)
  (define (weighted-average-entropy buckets)
    (/ (foldr (lambda(x y) (+ (* (get-entropy x) (length x)) y)) 0 buckets) (length (append* buckets))))
  (let* ([k (+ 1 (apply max (remove-duplicates (map f (map car data)))))]
         [emp (list-of-empty-lists k '())]
         [buckets (filter (lambda(x) (not (null? x))) (bucketing data emp f))])
    (- (get-entropy data) (weighted-average-entropy buckets))))


;choose the decision function that most reduces entropy of the data
(provide choose-f)
(define (choose-f candidates data) ; returns a decision function
  (argmax (lambda(f) (entropy-diff (cdr f) data)) candidates))

(provide DTree)
(struct DTree (desc func kids))

(define (probability-one data)
  (/ (count (lambda(x) (not (zero? x))) (map cdr data)) (length data)))
;build a decision tree (depth limited) from the candidate decision functions and data
(provide build-tree)
(define (build-tree candidates data depth)
  (if (or (null? candidates) (= depth 0))
      (DTree (~a (probability-one data)) 0 '())
      (let* ([f (choose-f candidates data)]
             [k (+ 1 (apply max (remove-duplicates (map (cdr  f) (map car data)))))]
             [emp (list-of-empty-lists k '())]
             [new-cands (remove f candidates)]
             [buckets (filter (lambda(x) (not (null? x))) (bucketing data emp (cdr f)))])
        (DTree (car f) (cdr f) (map (lambda(d) (build-tree new-cands d (- depth 1))) buckets)))))

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
