#lang racket

;;; This file provides convenience functions for working with data in
;;; Racket, including easier csv import (with heuristic-driven
;;; automatic ->number conversion when appropriate), and plotting
;;; functions.

;;; Dependencies
(require csv-reading)

;;; Can't live without alist-ref from Chicken scheme. Let's recreate
;;; it here, but with a shorter name
(define (aref idx lst)
  (cdr (assv idx lst)))

;;; Convenience csv reader that converts everything internally to
;;; numbers by default. It also igores lines in the input file
;;; commented with the "#" character
(define (read-csv file-path #:->number? [->number? #t])
  (let ((csv-reader (make-csv-reader-maker
		     '((comment-chars #\#))))) 
    (with-input-from-file (string->path file-path)
      (lambda ()
	(let ((tmp (csv->list (csv-reader (current-input-port)))))
	  (if ->number?
	      ;; try to convert everything to numbers rather than
	      ;; strings. This should be made smarter, converting only
	      ;; those columns which are actually numbers
	      (map (lambda (x) (map string->number x)) tmp)
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
