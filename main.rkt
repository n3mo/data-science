#lang racket

;;; This file provides convenience functions for working with data in
;;; Racket, including easier csv import (with heuristic-driven
;;; automatic ->number conversion when appropriate), and plotting
;;; functions.

;;; Dependencies
(require "./lexicons/nrc-lexicon")
(require "./lexicons/bing-lexicon")
(require "./lexicons/AFINN-lexicon")
(require "./lexicons/SMART-stopwords")
(require "./lexicons/snowball-stopwords")
(require "./lexicons/onix-stopwords")

(require csv-reading math math/matrix plot racket/hash)

(provide aref read-csv write-csv ci subset $ group-with aggregate sorted-counts
	 hist hist* scale log-base xs linear-model linear-model* chi-square-goodness
	 svd-1d cov document->tokens tdm dtm cosine-similarity
	 token->sentiment list->sentiment remove-urls
	 remove-punctuation remove-stopwords n-gram qq-plot qq-plot*
	 (all-from-out "./lexicons/nrc-lexicon"
		       "./lexicons/bing-lexicon"
		       "./lexicons/AFINN-lexicon"
		       "./lexicons/SMART-stopwords"
		       "./lexicons/snowball-stopwords"
		       "./lexicons/onix-stopwords"))

;;; Can't live without alist-ref from Chicken scheme. Let's recreate
;;; it here, but with a shorter name
(define (aref idx lst)
  (let ([tmp (assv idx lst)])
    (if tmp
	(cadr tmp)
	#f)))

;;; Convenience csv reader that can convert everything internally to
;;; numbers. It also igores lines in the input file commented with the
;;; "#" character. When header? is not #f, the first line of the file
;;; is assumed to contain column names.
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

;;; Embedded quotes (" or ') in csv files should be doubled. i.e.,
;;; " -> "" and ' -> ''
(define (string-cleaner strng)
  (string-append "\"" (regexp-replace* "\"" strng "\"\"") "\""))

;;; Missing, true/false, etc data can be adjusted here. Without this
;;; step, true/false data would be written to file as #t/#f and
;;; missing data as null. We could change those behaviors to
;;; TRUE/FALSE and NA here.
(define (format-csv-record record)
  (cond
   ;; [(eq? record #t) #t]
   ;; [(eq? record #f) #f]
   ;; [(eq? record 'null) null]
   [(string? record) (string-cleaner record)]
   [else record]))

;;; This writes a list of lists (record) to disk. 
(define (write-csv records file-path #:delimeter [delimeter #\,])
  (with-output-to-file file-path
    (λ ()
     ;; Outer loop across rows
     (for-each (lambda (row)
		 ;; Inner loop across columns
		 (let column-loop ((fields row))
		   (if (null? fields)
		       (newline)
		       (let ((curr-field (format-csv-record (car fields)))
			     (final? (null? (cdr fields))))
			 ;; (write curr-field)
			 (display curr-field)
			 (when (not final?) (display delimeter))
			 (column-loop (cdr fields))))))
	       records))))

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
(define (subset lst index f)
  (let ([F (if (or (string? f) (symbol? f) (number? f))
	       (λ (x) (equal? x f))
	       f)])
    (if (number? index)
	(if (or (< index 0) (>= index (length (car lst))))
	    (error "Invalid column number")
	    (filter (lambda (x) (F (list-ref x index))) lst))
	(let* ([header (car lst)]
	       [fn (cond
		    [(string? (car header)) string->symbol]
		    [(symbol? (car header)) identity]
		    [else (error "Header must be of type string or symbol")])])
	  (let* ([header-index (map list (map fn header) (range (length header)))]
		 [name (aref index header-index)])
	    (if name
		(cons (car lst) (filter (lambda (x) (F (list-ref x name))) (cdr lst)))
		(error "Invalid column name")))))))

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
  (discrete-histogram (sorted-counts lst)))

;;; Same as hist, but automatically passed the renderer to `plot`
;;; for quick convenience
(define (hist* lst)
  (plot (hist lst)
	#:x-label "Value"
	#:y-label "Frequency"))

;;; Pretty break points for histograms. Consider
;;; http://planspace.org/20141225-how_does_r_calculate_histogram_break_points/ 
;; (define (pretty-breaks lst)
;;   (let ([lo (apply min lst)]
;; 	[up (apply max lst)]
;; 	[rounding-eps 1e-7]
;; 	[dx (- up lo)]
;; 	[cell (max (abs lo) (abs up))])))

;;; This is a work in progress. The hist function above only works for
;;; discrete data, and even then not well. This function will
;;; eventually replace hist and hist*. It uses Sturge's formula to
;;; automatically determine the number of bins.
(define (better-hist s)
  (let* ([s-min (apply min s)]
	 [s-max (apply max s)]
	 [k (ceiling (+ (log-base (length s) #:base 2) 1))]	 
	 [step-size (/ (- s-max s-min) k)]
	 [bins (stream->list (in-range (- s-min step-size) s-max step-size))]
	 [bin-counts (map list bins (map sample-bin-total (bin-samples bins <= s)))])
    (discrete-histogram bin-counts)))


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

;;; Calculate logarithms with an arbitrary base
(define (log-base n #:base [base (exp 1)])
  (/ (log n) (log base)))

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

;;; Chi-square goodness of fit test. lst should contain variables and
;;; observered frequencies as a list of lists '(("Yes" 45) ("No"
;;; 37)). p should be a list of hypothesized probabilities, one for
;;; each variable in lst '(0.50 0.50). The alpha level (default 0.05)
;;; can be optionally set.
(define (chi-square-goodness lst p #:alpha [alpha 0.05])
  (let* ([observed ($ lst 1)]
	 [n (sum observed)]
	 [df (- (length lst) 1)]
	 [expected (map (λ (x) (* n x)) p)])
    (let ([chisqr (sum (map
			(λ (o e) (/ (expt (- o e) 2) e))
			observed expected))]
	  [criterion (inv-cdf (gamma-dist (/ df 2) 2) alpha #f null)])
      (if (> chisqr criterion)
	  (make-hash `(('chisqr . ,chisqr)
		       ('criterion . ,criterion)
		       ('alpha . ,alpha)
		       ('df . ,df)
		       ('result . "significant")))
	  (make-hash `(('chisqr . ,chisqr)
		       ('criterion . ,criterion)
		       ('alpha . ,alpha)
		       ('df . ,df)
		       ('result . "not-significant")))))))


;;; One-dimensional singular value decomposition using the "Power
;;; Method". This is used by `svd` to estimate a full singular value
;;; decomposition. Input `A` should be a matrix.
(define (svd-1d A [epsilon 1e-10])
  (let* ([threshold (- 1 epsilon)]
	 [m (matrix-num-cols A)]
	 [rand-norm (sample (normal-dist) m)]
	 [x-norm (sqrt (apply + (map sqr rand-norm)))]
	 [initial-vector (->col-matrix (map (λ (x) (/ x x-norm)) rand-norm))]
	 [B (matrix* (matrix-transpose A) A)])
    (let loop ([previous-v initial-vector])
      (let* ([pre-norm-current (matrix* B previous-v)]
	     [norm-div (make-matrix m 1 (matrix-norm pre-norm-current))]
	     [current-v (matrix-map / pre-norm-current norm-div)])
	(if (> (matrix-dot current-v previous-v) threshold)
	    current-v
	    (loop current-v))))))

;;; Covariance matrix for lists x and y
(define (cov x y)
  (let* ((xbar (mean x))
	 (ybar (mean y))
	 (xn (length x))
	 (yn (length y)))
    (list->matrix
     2 2
     `(,(/ (apply + (map (λ (x) (* (- x xbar) (- x xbar))) x))
	   (sub1 xn))
       ,(/ (apply + (map (λ (x y) (* (- x xbar) (- y ybar))) x y))
	   (sub1 xn))
       ,(/ (apply + (map (λ (x y) (* (- y ybar) (- x xbar))) x y))
	   (sub1 xn))
       ,(/ (apply + (map (λ (x) (* (- x ybar) (- x ybar))) y))
	   (sub1 yn))))))

;;; TEXT ANALYSIS TOOLS

;;; Remove URLs in a string
(define (remove-urls str)
  (regexp-replace* #px"http[s]?://[^ ]+\\b" str " "))

;;; Remove punctuation in a string (you should typically remove-urls
;;; before removing punctuation)
(define (remove-punctuation str #:websafe? [websafe? #f])
  (if websafe?
      (regexp-replaces str '([#px"&amp" " "]
			     [#px"\\|" " "]
			     [#px"[:;\\.,\\-=`~!]" ""]))
      (regexp-replace* #px"\\P{Ll}" str " ")))

;;; Remove stopwords. This procedure expects a list of words and a
;;; stopword lexicon of either 'SMART 'snowball or 'onix. It returns a
;;; list of non stop-words
(define (remove-stopwords lst #:lexicon [lexicon 'SMART])
  (cond [(equal? lexicon 'SMART)
	 (set-subtract lst SMART)]
	[(equal? lexicon 'snowball)
	 (set-subtract lst snowball)]
	[(equal? lexicon 'onix)
	 (set-subtract lst onix)]))

;;; Extract n-grams from string. Returns list of all possible n-grams
;;; of size `n` from the string `str`
(define (n-gram str n)
  (let loop ([words (string-split str)])
    (if (or (null? words)
	    (< (length (cdr words)) (sub1 n)))
	'()
	(cons (append (take words 1) (take (drop words 1) (sub1 n)))
	      (loop (cdr words))))))

;;; Convert text string into a list-of-lists counting the number of
;;; occurences for each token/word. 
(define (document->tokens str #:sort? [sort? #f])
  (let-values ([(x y) (count-samples (string-split str))])
    (if sort?
	(sort (map list x y) (λ (x y) (> (second x) (second y))))
	(map list x y))))

;;; Calculates the term-document matrix for the list of documents
;;; contained in corpus. Returns a list including an ordered list of
;;; terms (words) that correspond to the rows of the tdm that is also
;;; returned. corpus is one or more lists, with each list as returned
;;; by `document->tokens`
(define (tdm . corpus)
  ;;; Create a unique list of items
  (define (unique lst) (remove-duplicates lst))

  ;;; Create a hash of zeroed-out values. This is used to ensure that
  ;;; each document has a value of zero for words contained in other
  ;;; documents, but not in itself. 
  (define (make-zeros-hash keys)
    (let ([n (length keys)])
      (make-immutable-hash (map list keys (build-list n (λ (x) 0))))))

  ;;; Turn a single document hash into one that contains all words
  ;;; across all documents
  (define (add-missing-terms hsh all-words)
    (hash-union hsh
		(make-zeros-hash all-words)
		#:combine/key (λ (k v1 v2) v1)))

  ;; Construct the term-document-matrix
  (let* ([num-docs (length corpus)]
	 [all-words (unique (apply append (map (λ (x) ($ x 0)) corpus)))]
	 [tdm-hash
	  (apply
	   hash-union
	   (map (λ (document)
		  (add-missing-terms (make-immutable-hash document)
				     all-words))
		corpus)
	   #:combine/key (λ (k v1 v2) (append v1 v2)))])
    ;; We have the raw tdm counts. We normalize and turn into a tf-idf
    ;; matrix 
    (let* ([raw-tdm (list*->matrix (hash-values tdm-hash))]
	   ;; How frequently each terms appears in a document,
	   ;; normalized 
	   [tf (matrix-normalize-cols raw-tdm 1)]
	   ;; Number of documents containing each term
	   [docs-with-term (matrix-sum
			    (matrix-cols
			     (matrix-map (λ (x) (if (equal? x 0) 0 1))
					 raw-tdm)))]
	   ;; idf 
	   [temp (matrix-map
		  (λ (x) (log-base (/ num-docs x) #:base 10))
		  docs-with-term)]
	   [idf (matrix-augment (make-list num-docs temp))])
      (list
       ;; Ordered list of terms
       (hash-keys tdm-hash)
       ;; tf-idf, with row order matching the ordered list of terms
       ;; also returned
       (matrix-map * tf idf)))))

;;; Same as tdm, but with documents returned as rows and terms as columns.
(define (dtm . corpus)
  (let ([temp (apply tdm corpus)])
    ;; dtm is simply the transpose of the tdm
    (list
     ;; Ordered list of terms
     (first temp)
     ;; tf-idf in document-term-matrix format
     (matrix-transpose (second temp)))))

;;; Cosine similarity for two vectors (row matrices)
(define (cosine-similarity v1 v2)
  (/ (matrix-dot v1 v2)
     (* (sqrt (matrix-dot v1 v1))
	(sqrt (matrix-dot v2 v2)))))

;;; Convenience function for reading a corpus of files contained in a
;;; specified directory and converting each to a list of strings.
(define (directory->strings path [extensions '(".txt")])
  ;; Helper function for finding valid files
  (define (file-list path)
    (find-files (λ (x) (member (path-get-extension x)
			       (map string->bytes/utf-8 extensions)))
		(expand-user-path path)))
  (if (directory-exists? (expand-user-path path))
      (let ([file-list (file-list path)])
	(map (λ (file)
	       (file->string file))
	     file-list))
      ;; Hmm, directory doesn't seem to exist
      (raise-argument-error 'directory->strings
			    "valid-directory"
			    path)))


;;; SENTIMENT ANALYSIS TOOLS

;;; Sentiment lexicons
;; (define nrc (with-input-from-file "./lexicons/nrc-lexicon" (λ () (read))))
;; (define bing (with-input-from-file "./lexicons/bing-lexicon" (λ () (read))))
;; (define AFINN (with-input-from-file "./lexicons/AFINN-lexicon" (λ () (read))))


;;; Convert a token/word into a sentiment score. Lexicon can be either
;;;   (1) 'nrc   : returns emotional labels
;;;   (2) 'bing  : returns "positive" or "negative"
;;;   (3) 'AFINN : returns a -4 to +4 numerical score
(define (token->sentiment token #:lexicon [lexicon 'nrc])
  (cond [(equal? lexicon 'nrc)
	 (let ([results (subset nrc 'word (λ (x) (string=? x token)))])
	   (if (> (length results) 1)
	       (drop (map (λ (x) (list (first x) (second x))) results) 1)
	       '()))]
	[(equal? lexicon 'bing)
	 (let ([results (subset bing 'word (λ (x) (string=? x token)))])
	   (if (> (length results) 1)
	       (drop (map (λ (x) (list (first x) (second x))) results) 1)
	       '()))]
	[else
	 (let ([results (subset AFINN 'word (λ (x) (string=? x token)))])
	   (if (> (length results) 1)
	       (drop (map (λ (x) (list (first x) (string->number (fourth x)))) results) 1)
	       (list (list token 0))))]))

;;; Takes as input a list of pairs of the type (string number), such
;;; as output by document->tokens where string is a word from a text
;;; and number is the number of times it occurs in the
;;; document. Lexicon can be either
;;;   (1) 'nrc   : returns emotional labels
;;;   (2) 'bing  : returns "positive" or "negative"
;;;   (3) 'AFINN : returns a -4 to +4 numerical score
(define (list->sentiment lst #:lexicon [lexicon 'nrc])
  (define (pack-sentiment lst lexicon)
    (apply append (list '("word" "sentiment" "freq"))
	   (map (λ (x) 
		  (let ([result (token->sentiment (first x) #:lexicon lexicon)])
		    (map (λ (y) (append y (list (second x)))) result)))
		lst)))
  (let ([sentiment (pack-sentiment lst lexicon)])
    (if (> (length sentiment) 1)
	sentiment
	'())))

;;; Example workflow:
;;; Read a document from file into a string:
;;;    (define doc (file->string "my-file.txt"))
;;; Count number of each token/word
;;;    (define words (document->tokens doc #:sort? #t))
;;; Determine total sentiment score
;;;    (apply + (list->sentiment words #:lexicon 'AFINN))
;;;
;;; OR, count positive/negative occurences
;;;   (sorted-counts (filter (λ (x) x) (list->sentiment words #:lexicon 'bing)))
;;; or plot the breakdown
;;;   (plot (discrete-histogram
;;;       (sorted-counts (filter (λ (x) x) (list->sentiment words #:lexicon 'bing)))))
;;;
;;; OR plot breakdown of emotional labels using nrc lexicon
;;;     (parameterize ((plot-width 800))
;;;       (plot (discrete-histogram
;;;           (sorted-counts (filter (λ (x) x) (list->sentiment words #:lexicon 'nrc))))))

;;; End of file data-science.rkt

