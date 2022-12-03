#lang racket

(define (read-input filename)
  (map string->list (file->lines filename)))

(define (score items)
  (for/sum ([it items])
    (cond
     [(char-lower-case? it) (- (char->integer it) 96)]
     [(char-upper-case? it) (+ 27 (- (char->integer it) 65))])))

(define (part1 in)
  (score
    (for/list ([l in])
      (define-values (a b) (split-at l (/ (length l) 2)))
      (car (set-intersect a b)))))

(define (part2 in)
  (let lp ([l in]
	   [res '()])
    (match l
      ['() (score res)]
      [(list-rest a b c rest)
       (lp rest (cons (car (set-intersect a b c)) res))])))

(define in (read-input "input"))
(displayln (part1 in))
(displayln (part2 in))
