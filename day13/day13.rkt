#lang racket

(require threading
	 rebellion/base/comparator
	 rebellion/collection/list
	 rebellion/streaming/transducer
	 rebellion/streaming/reducer)

(define read-pairs
  (lambda~> file->string
	    (string-split _ "\n\n")
	    (map (lambda (s) (string-split s "\n")) _)
	    (map (lambda (l) (map parse-packet l)) _)))

(define read-all
  (lambda~> file->string
	    string-split
	    (map parse-packet _)))

(define parse-packet
  (lambda~> (string-replace _ "," " ")
	    (with-input-from-string _ read)))

(define (packet-compare a b)
  (match (list a b)
    [(list x y) #:when (and (integer? x) (integer? y))
     (cond
      [(< x y) lesser]
      [(> x y) greater]
      [else equivalent])]
    [(list '() '()) equivalent]
    [(list '() _) lesser]
    [(list _ '()) greater]
    [(list (list* x xs) (list* y ys))
     (let ([compxy (packet-compare x y)])
       (if (eq? compxy equivalent)
	   (packet-compare xs ys)
	   compxy))]
    [(list x y) #:when (and (integer? x) (list? y))
     (packet-compare (list x) y)]
    [(list x y) #:when (and (list? x) (integer? y))
     (packet-compare x (list y))]))

(define packet<=>
  (make-comparator packet-compare #:name 'packet<=>))

(define (part1 in)
  (for/sum ([pair in]
	    [i (in-naturals)]
	    #:when (eq? lesser (apply packet-compare pair)))
    (+ 1 i)))

(define (part2 in)
  (for/product ([el (transduce (list* '((2)) '((6)) in)
			       (sorting packet<=>)
			       #:into into-list)]
		[i (in-naturals)]
		#:when (or (equal? el '((2)))
			   (equal? el '((6)))))
    (+ 1 i)))

(module+ main
  (displayln (part1 (read-pairs "input")))
  (displayln (part2 (read-all "input"))))

