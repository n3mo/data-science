#lang racket

;;; This file provides convenience functions for working with data in
;;; Racket, including easier csv import (with heuristic-driven
;;; automatic ->number conversion when appropriate), and plotting
;;; functions.

;;; Dependencies
(require csv-reading math plot)

;;; Can't live without alist-ref from Chicken scheme. Let's recreate
;;; it here, but with a shorter name
(define (aref idx lst)
  (let ([tmp (assv idx lst)])
    (if tmp
	(cadr tmp)
	#f)))

;;; Convenience csv reader that converts everything internally to
;;; numbers by default. It also igores lines in the input file
;;; commented with the "#" character. When header? is not #f, the
;;; first line of the file is assumed to contain column names.
(define (read-csv file-path
		  #:->number? [->number? #f]
		  #:header? [header? #t])
  (let ((csv-reader (make-csv-reader-maker
		     '((comment-chars #\#))))) 
    (with-input-from-file file-path
      (lambda ()
	(let* ((tmp (csv->list (csv-reader (current-input-port)))))
	  (if ->number?
	      ;; try to convert everything to numbers rather than
	      ;; strings. This should be made smarter, converting only
	      ;; those columns which are actually numbers
	      (if header?
		  (cons (car tmp) (map (lambda (x) (map string->number x)) (cdr tmp)))
		  (map (lambda (x) (map string->number x)) tmp))
	      ;; Else, leave everything as strings
	      tmp))))))

;;; Extract a particular column of data from a list of lists by
;;; (c)olumn (i)index number
(define (ci idx lsts)
  (map (lambda (x) (list-ref x idx)) lsts))

;;; We need to be able to logically index one column against
;;; another. Assuming a list of lists "d" containing our data, we can
;;; grab only those rows where the 5th column == 1118. The resulting
;;; indexed list of lists can then be fed into other procedures such
;;; as "ci"
;;; Example: (filter (lambda (x) (= 1118 (list-ref x 5))) d)

;;; This provides subsetting of data based on the value of a given
;;; column. To subset based on name, create a name-idx alist and then
;;; use (aref idx lst) in place of "col"
(define (cidx lsts col val)
  (filter (lambda (x) (equal? (list-ref x col) val)) lsts))

;;; Uninformative (i.e., bad) name for a function that allows you to
;;; extract columns from columnar data (list-of-lists). The short
;;; (i.e., good) name helps to avoid complex and overly-long commands
;;; to do this manually. This function expects that the first "row" of
;;; data contains the column names, or "header," and the remaining
;;; rows contain the data. Row names are accessed as symbols even if
;;; the actual data contains string headers. `lst` is a list-of-lists,
;;; as created, for example, by csv->list or read-csv. `name` is a
;;; symbol of a valid column name contained in the car of `lst`. Only
;;; the non-header rows of the requested column are returned.
(define ($ lst name)
  (let* ([header (car lst)]
	 [fn (cond
	      [(string? (car header)) string->symbol]
	      [(symbol? (car header)) identity]
	      [else (error "Header must be of type string or symbol")])])
    (let* ([header-index (map list (map fn header) (range (length header)))]
	   [idx (aref name header-index)])
      (if idx
	  (map (λ (x) (list-ref x idx)) (cdr lst))
	  (error "Invalid column name")))))

;;; The group-by included with Racket doesn't quite do what we
;;; want. We want to be able to take TWO lists, and group list-2 using
;;; the elements of list-1 as grouping factors. The following function
;;; provides this. If include-factors? is #t, each factor is included
;;; as the first element in each grouped sub-list. This enables the
;;; returned lists to be accessed as an alist.
(define (group-with factors lst [include-factors? #t])
  (let ((tmp (group-by (lambda (x) (car x)) (map list factors lst))))
    (if include-factors?
	(map (lambda (x) (append (list (caar x)) (map second x))) tmp)
	(map (lambda (x) (map second x)) tmp))))

;;; Aggregate. This is meant to behave similarly to R's aggregate
;;; function
(define (aggregate f factors lst)
  (let ((tmp (group-with factors lst)))
    (map (lambda (x) (list (car x) (f (cdr x)))) tmp)))

;;; Generating discrete histograms of (sorted!) binned samples should
;;; be easier. The following generates sorted bins suitable for
;;; plotting with `discrete-histogram`. This function is similar to
;;; `samples->hash` but does not return a hash or dotted pairs. The
;;; return value is a list of (key value) pairs sorted by keys.
;;; Example: '(3 3 2 1 4 4 4) => '((1 1) (2 1) (3 2) (4 3))
(define (sorted-counts lst)
  (let-values ([(keys values) (count-samples lst)])
    (sort (map list keys values)
	  (λ (x y) (if (number? (car x))
		       (< (car x) (car y))
		       (string<? (car x) (car y)))))))

;;; This recreates the `hist` function from R. Use this for
;;; quick and dirty histograms. If you want control over the plot's
;;; styling, call `sorted-counts` manually and pass the result to
;;; `discrete-histogram` yourself.
(define (hist lst)
  (plot (discrete-histogram (sorted-counts lst))))

;;; When you have a single list of values, it is useful to be able to
;;; plot the data as y-values. This requires creating token x values
;;; with (range (length ys)). Better to make this a short call for
;;; convenience. Given a list of y values, this returns (x1, y1) pairs
;;; for plotting
(define (xs ys)
  (map list (range (length ys)) ys))

;;; Simple linear least squares regression: y = mx + b
;;; Returns two values (slope intercept) 
;;; Example: Given a list of x values and y values:
;;;
;;; (define xs (range 100))
;;; (define ys (map + xs (sample (normal-dist 0 30) 100)))
;;; (let* ([coef (linear-regression units minutes)]
;;;        [slope (car coef)]
;;;        [intercept (cdr coef)])
;;;   (plot (list (points (map vector units minutes))
;;; 	      (function (λ (x) (+ (* x slope) intercept))))))
(define (linear-regression xs ys)
  (let* ([x-hat (mean xs)]
	 [y-hat (mean ys)]
	 [x-devs (map (λ (x) (- x x-hat)) xs)]
	 [y-devs (map (λ (x) (- x y-hat)) ys)]
	 [numerator (apply + (map * x-devs y-devs))]
	 [x-sqr (apply + (map (λ (x) (expt x 2)) x-devs))]
	 [slope (/ numerator x-sqr)])
    (list slope (- y-hat (* slope x-hat)))))
