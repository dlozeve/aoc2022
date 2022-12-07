#lang racket

(struct tree
  (name parent type (size #:mutable) (children #:mutable))
  #:transparent)

(define (read-input filename)
  (define t (tree "/" #f 'dir #f '()))
  (let lp ([current-dir t]
	   [lines (cdr (file->lines filename))])
    (unless (null? lines)
      (match (string-split (car lines))
	[(list "$" "ls")
	 (lp current-dir (cdr lines))]
	[(list "$" "cd" "..")
	 (lp (tree-parent current-dir) (cdr lines))]
	[(list "$" "cd" name)
	 (let ([child (findf (lambda (x) (equal? name (tree-name x)))
			     (tree-children current-dir))])
	   (if child
	       (lp child (cdr lines))
	       (let ([child (tree name current-dir 'dir #f '())])
		 (set-tree-children! current-dir (cons child (tree-children current-dir)))
		 (lp child (cdr lines)))))]
	[(list "dir" _)
	 (lp current-dir (cdr lines))]
	[(list size name)
	 (let ([child (tree name current-dir 'file (string->number size) '())])
	   (set-tree-children! current-dir (cons child (tree-children current-dir)))
	   (lp current-dir (cdr lines)))])))
  (compute-sizes! t)
  t)

(define (compute-sizes! t)
  (if (tree-size t)
      (tree-size t)
      (begin
	(set-tree-size! t (apply + (map compute-sizes! (tree-children t))))
	(tree-size t))))

(define (display-tree t (level 0) (out (current-output-port)))
  (fprintf out "~a- ~a (~a, size=~a)~n"
	   (make-string (* 2 level) #\ )
	   (tree-name t)
	   (tree-type t)
	   (tree-size t))
  (for ([child (sort (tree-children t) string<=? #:key tree-name)])
    (display-tree child (+ 1 level) out)))

(define (file? t)
  (eq? 'file (tree-type t)))

(define (directory? t)
  (eq? 'dir (tree-type t)))

(define (all-directories t)
  (define child-dirs (filter directory? (tree-children t)))
  (append* child-dirs (map all-directories child-dirs)))

(define (part1 t)
  (apply + (map tree-size
		(filter (lambda (d) (< (tree-size d) 100000))
			(all-directories t)))))

(define (part2 t)
  (define unused-space (- 70000000 (tree-size t)))
  (define required-space (- 30000000 unused-space))
  (define dir-to-remove
    (car (sort (filter (lambda (d) (>= (tree-size d) required-space))
		       (all-directories t))
	       <=
	       #:key (lambda (d) (tree-size d)))))
  (tree-size dir-to-remove))

(module+ main
  (define t (read-input "input"))
  (displayln (part1 t))
  (displayln (part2 t)))
