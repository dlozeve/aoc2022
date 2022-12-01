#lang racket

(require threading)

(define (read-input filename)
  (for/list ([s (string-split (file->string filename) "\n\n")])
    (for/list ([n (string-split s "\n")])
      (string->number n))))

(define (sum l)
  (apply + l))

(define part1
  (lambda~>> (map sum)
	     (apply max)))

(define part2
  (lambda~> (map sum _)
	    (sort _ >)
	    (take _ 3)
	    (apply + _)))

(define input (read-input "input"))
(displayln (part1 input))
(displayln (part2 input))
