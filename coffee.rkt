#lang plai-typed

(define-type PotState (pot-empty) (cup (p : PotState)))
(define-type MugState (mug-empty) (mug-full))

(define (pint [ps : PotState]) (cup (cup ps)))
(define (quart [ps : PotState]) (pint (pint ps)))
(define (ten-cups [ps : PotState]) (quart (quart (pint ps))))

(define (pour-cup [ps : PotState] [ms : MugState])
  (type-case PotState ps
             [pot-empty () (values (pot-empty) ms)]
             [cup (p) 
                  (values p 
                          (type-case MugState ms 
                                     [mug-empty () (mug-full)]
                                     [mug-full () (error 'mug-overflow "Disaster!!")]))]))

(define (brew-pot [ps : PotState])
  (type-case PotState ps
             [pot-empty () (ten-cups (pot-empty))]
             [cup (p) (error 'pot-overflow "Disaster!!")]))

(define (drink-mug [ms : MugState]) (mug-empty))
(define (drink-pot?! [ps : PotState]) 
  (type-case PotState ps
             [pot-empty () (pot-empty)]
             [cup (p) p]))
