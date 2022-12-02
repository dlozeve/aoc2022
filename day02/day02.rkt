#lang racket

(require threading)

(define (read-input filename)
  (for/list ([l (in-lines (open-input-file filename))])
    (map string->symbol (string-split l))))

(define (win-score round)
  (~> (- (cadr round) (car round))
      (+ 1 _)
      (modulo _ 3)
      (* 3 _)))

(define (shape-score round)
  (+ 1 (cadr round)))

(define (read-play s)
  (match s
    ['A 0]
    ['B 1]
    ['C 2]
    ['X 0]
    ['Y 1]
    ['Z 2]
    [_ (error "unrecognized symbol" s)]))

(define (part1 in)
  (for/sum ([round in])
    (let ([round (map read-play round)])
      (+ (shape-score round) (win-score round)))))

(define (choose-play round)
  (define them (read-play (car round)))
  (define us
    (modulo
     (match (cadr round)
       ['X (- them 1)]
       ['Y them]
       ['Z (+ them 1)]
       [_ (error "unexpected symbol" (cadr round))])
     3))
  (list them us))

(define (part2 in)
  (for/sum ([round in])
    (let ([round (choose-play round)])
      (+ (shape-score round) (win-score round)))))

(define in (read-input "input"))
(displayln (part1 in))
(displayln (part2 in))
