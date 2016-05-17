#lang racket

;;; This file provides convenience functions for working with data in
;;; Racket, including easier csv import (with heuristic-driven
;;; automatic ->number conversion when appropriate), and plotting
;;; functions.

;;; Dependencies
(require csv-reading math math/matrix plot)
(provide aref read-csv ci cidx $ group-with aggregate sorted-counts
	 hist xs linear-model)

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
;;; to do this manually. This function expects either (1) that index
;;; is a number, in which case the corresponding "column" is returned,
;;; or (2) that index is a symbol. In this situation the first "row"
;;; of data contains the column names, or "header," and the remaining
;;; rows contain the data. Row names are accessed as symbols even if
;;; the actual data contains string headers. `lst` is a list-of-lists,
;;; as created, for example, by csv->list or read-csv. `name` is a
;;; symbol of a valid column name contained in the car of `lst`. Only
;;; the non-header rows of the requested column are returned.
(define ($ lst index)
  (if (number? index)
      (if (or (< index 0) (>= index (length (car lst))))
	  (error "Invalid column number")
	  (map (λ (x) (list-ref x index)) lst))
   (let* ([header (car lst)]
	  [fn (cond
	       [(string? (car header)) string->symbol]
	       [(symbol? (car header)) identity]
	       [else (error "Header must be of type string or symbol")])])
     (let* ([header-index (map list (map fn header) (range (length header)))]
	    [name (aref index header-index)])
       (if name
	   (map (λ (x) (list-ref x name)) (cdr lst))
	   (error "Invalid column name"))))))

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

;;; Simple z-transformation. Scales data to mean = 0 and stddev = 1
(define (scale lst)
  (let ([lst-mean (mean lst)]
	[lst-stddev (stddev lst)])
    (map (λ (x) (/ (- x lst-mean) lst-stddev)) lst)))

;;; Q-Q Plot. Plots sample quantiles against theoretical quantiles
;;; from a normal distribution with a mean and standard deviation
;;; of the sample `lst`. By default, both quantiles are
;;; z-transformed. Suppress this behavior with #:scale? #f. Returns a
;;; renderer for use with `plot`, `plot-file`, etc.
(define (qq-plot lst #:scale? [scale? #t])
  (let* ([n (length lst)]
	 [lst-mean (mean lst)]
	 [lst-stddev (stddev lst)]
	 [probs (map (λ (x) (/ x (+ 2 n))) (range 1 (add1 n)))]
	 [normal-quantiles
	  (map (λ (x) (inv-cdf (normal-dist lst-mean lst-stddev) x)) probs)]
	 ;; Scale the data?
	 [xs (if scale? (scale normal-quantiles) normal-quantiles)]
	 [ys (if scale? (scale lst) lst)])
    (points (map vector (sort-samples < xs)
		 (sort-samples < ys)))))

;;; Same as qq-plot, but automatically passed the renderer to `plot`
;;; for quick convenience
(define (qq-plot* lst #:scale? [scale? #t])
  (plot (qq-plot lst #:scale? scale?)
	#:x-label "Theoretical Normal Quantiles"
	#:y-label "Sample Quantiles"))


;;; Regression solver using linear algebra.
;;; Returns '(intercept coefficient-1 coefficient-2 ...)
;;; Example 1: Simple Linear Regression
;;; Given a list of x values and y values:
;;;
;;; (define xs (range 100))
;;; (define ys (map + xs (sample (normal-dist 0 30) 100)))
;;; (let* ([coef (linear-model xs ys)]
;;;        [slope (cadr coef)]
;;;        [intercept (car coef)])
;;;   (plot (list (points (map vector xs ys))
;;; 	      (function (λ (x) (+ (* x slope) intercept))))))
;;;
;;; Example 2: Multiple linear regression.
;;; Multiple (additive) predictors can be used. With two predictors,
;;; X1 & X2, each predictor should be a "column" in a list-of-list,
;;; such as '((X1 X2) (X1 X2) ...). Say we have two predictors, X1 and
;;; X2, and observed outcome Y:
;;;
;;; (define x1 '(52 59 67 73 64 74 54 61 65 46 72))
;;; (define x2 '(173 184 194 211 196 220 188 188 207 167 217))
;;; (define y '(132 143 153 162 154 168 137 149 159 128 166))
;;; (linear-model (map list x1 x2) y)
;;; ;;; Plot a surface?
;;; (let* ([coef (linear-model (map list x1 x2) y)]
;;;        [intercept (first coef)]
;;;        [b1 (second coef)]
;;;        [b2 (third coef)])
;;;   (plot3d (list (points3d (map vector x1 x2 y))
;;; 		(surface3d (λ (x1 x2) (+ intercept (* b1 x1) (* b2 x2)))))))
(define (linear-model xs y)
  (let ([X (list*->matrix
	    (map (λ (x y) (flatten (list x y)))
		 (build-list (length xs) (const 1)) xs))]
	[Y (->col-matrix y)])
    ;; We solve for A, a col-matrix containing [intercept slope]
    ;; A = ((X^TX)^-1)X^TY
    ;; Where X^T means transpose of X, and ^-1 means inverse
    (matrix->list (matrix*
		   (matrix-inverse (matrix* (matrix-transpose X) X))
		   (matrix* (matrix-transpose X) Y)))))

;;; Version with rich output. Model parameters and inputed data
;;; (packaged as matrices) returned as a hash
(define (linear-model* xs ys)
  (let ([X (list*->matrix
	    (map (λ (x y) (flatten (list x y)))
		 (build-list (length xs) (const 1)) xs))]
	[Y (->col-matrix ys)])
    ;; We solve for A, a col-matrix containing [intercept slope]
    ;; A = ((X^TX)^-1)X^TY
    ;; Where X^T means transpose of X, and ^-1 means inverse
    (let* ([coef (matrix*
		  (matrix-inverse (matrix* (matrix-transpose X) X))
		  (matrix* (matrix-transpose X) Y))]
	   [residuals (matrix- Y (matrix* X coef))]
	   [n (matrix-num-rows X)]
	   [p (sub1 (matrix-num-cols X))]
	   [mse (/ (matrix-ref
		    (matrix* 
		     (matrix-transpose (matrix- Y (matrix* X coef)))
		     (matrix- Y (matrix* X coef))) 0 0)
		   (- n p))]
	   [root-mse (sqrt mse)])
      ;; Return a hash of model results
      (hash 'X X 'Y Y
	    'coef (matrix->list coef)
	    'residuals (matrix->list residuals)
	    'n n
	    'p p
	    'mse mse
	    'root-mse root-mse))))

;;; End of file data-science.rkt
