#lang racket

(require data/queue
	 threading)

(struct monkey
  (items op test divisor (inspections #:mutable))
  #:transparent)

(define (read-input filename worry-reduction)
  (for/vector ([mstr (string-split (file->string filename) "\n\n")])
    (define infos (string-split mstr "\n"))
    (define items (make-queue))
    (for ([item (map string->number (string-split (substring (cadr infos) 18) ", "))])
      (enqueue! items item))
    (define op
      (compose1
       (lambda (n) (quotient n worry-reduction))
       (match (string-split (substring (caddr infos) 23))
	 [(list "+" "old") (lambda (x) (+ x x))]
	 [(list "*" "old") (lambda (x) (* x x))]
	 [(list "+" n) (lambda (x) (+ x (string->number n)))]
	 [(list "*" n) (lambda (x) (* x (string->number n)))])))
    (match-define (list d a b) (map (compose1 string->number last string-split) (cdddr infos)))
    (define (test n)
      (if (zero? (remainder n d)) a b))
    (monkey items op test d 0)))

(define (throw! item to)
  (enqueue! (monkey-items to) item))

(define (process-item! ms m (mod #f))
  (set-monkey-inspections! m (+ 1 (monkey-inspections m)))
  (define item ((monkey-op m) (dequeue! (monkey-items m))))
  (when mod
    (set! item (remainder item mod)))
  (throw! item (vector-ref ms ((monkey-test m) item))))

(define (run-round! ms (mod #f))
  (let loop ([i 0])
    (cond
     [(= i (vector-length ms)) ms]
     [(queue-empty? (monkey-items (vector-ref ms i))) (loop (+ 1 i))]
     [#t (begin (process-item! ms (vector-ref ms i) mod)
		(loop i))])))

(define (run-rounds! ms n (mod #f) (display? #f))
  (for ([i (in-range n)])
    (run-round! ms mod)
    (when display? (display-items ms))))

(define (display-items ms)
  (for ([m ms]
	[i (in-naturals)])
    (printf  "Monkey ~a: ~a~n"
	     i
	     (string-join (map number->string (queue->list (monkey-items m))) ", "))))

(define (inspection-levels ms)
  (vector->list (vector-map monkey-inspections ms)))

(define (monkey-business inspections)
  (~> inspections
      (sort >=)
      (take 2)
      (apply * _)))

(module+ main
  (let* ([ms (read-input "input" 3)])
    (run-rounds! ms 20)
    (displayln (monkey-business (inspection-levels ms))))
  (let* ([ms (read-input "input" 1)]
	 [mod (apply * (vector->list (vector-map monkey-divisor ms)))])
    (run-rounds! ms 10000 mod)
    (displayln (monkey-business (inspection-levels ms)))))
