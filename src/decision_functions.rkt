#lang racket

;candidate functions for the toy dataset
(provide y1)
(provide y2)
(provide y3)
(provide y4>62)

(define y1 (cons "feature1" first)) ; returns the value of feature 1 for a given test sample
(define y2 (cons "feature2" second))
(define y3 (cons "feature3" third))
(define y4>62 (cons "feature4>62" (lambda (x) (if (> (fourth x) 62) 1 0)))) ; returns 1 if the value of feature 4 > 62, else 0

;candidate functions for the titanic dataset
(provide pclass)
(provide sex)
(provide age>25)
(provide sibsp)
(provide parch)
(provide fare>50)
(provide emb)

(define pclass (cons "pclass" first)) ; returns the value of pclass for a given test sample
(define sex (cons "sex" second))
(define age>25 (cons "age>25" (lambda(x) (if (> (third x) 25) 1 0))))
(define sibsp (cons "sibsp" fourth))
(define parch (cons "parch" fifth))
(define fare>50 (cons "fare>50" (lambda(x) (if (> (sixth x) 50) 1 0))))
(define emb (cons "emb" seventh))

;candidate functions for the mushroom dataset
(provide cshape)
(provide csurf)
(provide bruise)
(provide odor)
(provide gatch)
(provide gspace)
(provide gsize)
(provide sshape)
(provide nring)
(provide pop)
(provide hab)

(define cshape (cons "cshape" first))
(define csurf (cons "csurf" second))
(define bruise (cons "bruise" third))
(define odor (cons "odor" fourth))
(define gatch (cons "gatch" fifth))
(define gspace (cons "gspace" sixth))
(define gsize (cons "gsize" seventh))
(define sshape (cons "sshape" eighth))
(define nring (cons "nring" ninth))
(define pop (cons "pop" tenth))
(define hab (cons "hab" (lambda(x) (list-ref x 10))))
