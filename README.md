# Data Science Tooling For Racket

This project is a work in progress. Documented functions are working, however, and you are welcome to snoop around. Eventually, this will evolve into a package of convenience functions for data science: data cleaning, wrangling, summarizing, and exploration. 

Many functions contained within are inspired by functionality commonly available in more statistically-oriented software packages such as R, numpy/scipy, and Matlab/Octave. This all began because I wanted the R function `aggregate` in Racket.

# Table of Contents

- [Installation](#installation)
- [Data Import/Export](#importexport)
 - [CSV Reading](#read-csv)
 - [CSV Writing](#write-csv)
- [Split -> Apply -> Combine Workflows](#split-apply-combine)
 - [Column Indexing](#-column-indexing)
 - [Aggregate](#aggregate)
 - [Grouping Data](#group-with)
 - [Counting Samples](#sorted-counts)
 - [Subsetting Data](#subset)
- [Statistical Utilities](#general-utilities)
 - [Logarithms](#log-base-logarithms-with-arbitrary-base)
 - [Singular Value Decomposition](#singular-value-decomposition-1-dimensional)
 - [Data scaling: Z-transformation](#z-transform-data-scale)
- [Statistical Tests/Models](#statistical-tests-and-models)
 - [Linear Regression Models](#linear-model)
 - [Chi-Square Goodness of Fit Test](#chi-square-goodness)
- [Text Processing](#text-processing)
 - [URL Removal](#remove-urls)
 - [Punctuation Removal](#remove-punctuation)
 - [Stop-word Removal](#remove-stopwords)
 - [n-gram Extraction](#n-gram)
 - [Tokenizing Documents](#document-tokens)
 - [Document Term Matrix](#dtm)
 - [Term Document Matrix](#tdm)
 - [Cosine Similarity](#cosine-similarity)
- [Sentiment Analysis](#sentiment-analysis)
 - [Token/word sentiment](#token-sentiment)
 - [Document Sentiment](#list-sentiment)
- [Plotting Utilities](#plotting-functions)
 - [Frequency Histograms](#histogram-of-sorted-counts)
 - [Quantile-Quantile (Q-Q) Plot](#quantile-quantile-q-q-plot)
- [Bugs and Improvements](#bugs--improvements)
- [License](#license)

## Installation
The data-science package is not currently registered in Racket's package catalog. It can, however, be installed directly from GitHub via raco:

``` racket
raco pkg install https://github.com/n3mo/data-science.git
```

# Usage

## Import/Export

### read-CSV

```racket
(read-csv file-path [#:->number? #f
                     #:header? #t])
```

Convenience wrapper around `csv->list` and `make-csv-reader` from the *csv-reading* package. Reads a comma-separated-values file located at `file-path`, ignoring lines beginning with the # character. If *#:->number* is #t, `read-csv` attempts to convert the data to numbers, otherwise everything is treated as strings. When *#:header?* is #t, the first line in the input file is assumed to contain column names.

Example:

```racket
;;; Read a data file, converting to numeric data type
(define data (read-csv "./my_data_file.csv" #:->number? #t))
```

### write-CSV

```racket
(write-csv lst file-path [#:delimeter #\,])
```

Writes a list-of-lists to file-path as comma-separated-values. The default delimeter is a comma.

Example:

```racket
(define my-data '(("ship" "anticipation" 518)
		  ("sea" "positive" 455)
		  ("long" "anticipation" 334)
		  ("time" "anticipation" 334)
		  ("captain" "positive" 329)))

(write-csv my-data "./data-results.csv")
```

## Split->Apply->Combine
`data-science` provides a collection of ultility functions for breaking your data into meaningful pieces, applying functions to each piece, and then recombining the results. In fact, the filter/map/apply approach of lisp-like languages is well suited to such tasks. However, with complex analyses, commands can grow quite complex and cumbersome, and can mask their intended purpose. The following functions provide convenient short-hand procedures that aim to be *expressive, yet concise*.

### $ (Column-indexing)

``` racket
($ lst index)
```

Returns a "column" of data from a list-of-lists *lst*. Column selection is controlled by *index,* which can be either a number or a symbol. If *index* is a number, the corresponding column is returned from all rows of *lst*. If *index* is a symbol, then the first row of *lst* is assumed to be a header containing named fields of each column. In this situation, $ returns the corresponding column of data identified by the column name, *excluding the first row*--that is, the header name is **not** part of the return value.

Examples:

``` racket
;;; Numerical indexing
(define my-data '((1 2 3) 
                  (4 5 6) 
                  (7 8 9) 
                  (10 11 12)))
($ my-data 0)
;; --> '(1 4 7 10)

;;; Indexing by name
(define my-data '((age rank id) 
                  (21 1 100) 
                  (18 2 101) 
                  (32 1 102) 
                  (19 4 103)))
($ my-data 'rank)
;; --> '(1 2 1 4)
```

### Aggregate

```racket
(aggregate f factors lst)
```

`aggregate` applies function `f` to the elements of `lst`, as grouped by `factors`. Essentially, `aggregate` groups the data using the function `group-with` (see documentation for full details), and applies the function `f` to each grouping.

Examples:

```racket
;;; A little help from the math module
(require math)

;;; Say that we have responses from two experimental conditions, 
;;; "A" and "B," and we want to know about these two groups separately. 

;;; Our condition factors
(define condition '(A A A B B B B B B A A))

;;; Our response data
(define response '(3 5 5 1 2 1 4 7 3 8 3))

;;; How many observations from each condition?
(aggregate length condition response)
;; --> '((A 5) (B 6))

;;; What's the mean response from each condition?
(aggregate mean condition response)
;; --> '((A 24/5) (B 3))

;;; Let's add 1 to each response from each condition
(aggregate (λ (x) (map add1 x)) condition response)
;; --> '((A (4 6 6 9 4)) (B (2 3 2 5 8 4)))
```

### Group-With

```racket
(group-with factors lst [include-factors? #t])
```

Similar to `group-by`, but accepts two *ordered* lists: `factors` and `lst`. The elements of `lst` are grouped into sub-lists according to the grouping factors in `factors`. Returns a list of lists, with one list per element in `factors`. If *include-factors?* is #t, the grouping element from `factors` is included first in each sub-list, enabling the returned lists to be accessed as an associate list.

Examples:

```racket
(group-with '(A B A B A B A B) '(1 2 3 4 5 6 7 8))
;; --> '((A 1 3 5 7) (B 2 4 6 8))

;;; Or, with no factors returned:
(group-with '(A B A B A B A B) '(1 2 3 4 5 6 7 8) #f)
;; --> '((1 3 5 7) (2 4 6 8))
```

### Sorted-Counts

```racket
(sorted-counts lst)
```

Data in `lst` are counted and sorted into a list of observed frequencies. Returns a list-of-lists suitable for passing to `discrete-histogram`. In fact, `hist` and `hist*` call `sorted-counts` to tabulate data prior to plotting. 

### Subset

```racket
(subset lst index f)
```

Returns (filters) a subset of data from a list-of-lists *lst*. Column selection is controlled by *index,* which can be either a number or a symbol. If *index* is a number, data from the corresponding column is used. If *index* is a symbol, then the first row of *lst* is assumed to be a header containing named fields of each column. In this situation, data from the corresponding column of data identified by the column name is used, *excluding the first row*--that is, the header row is not subjected to filtering, and is returned un-touched as part of the returned subset. In either case, the values from the indexed column are filtered via *f*, which is either a 1-parameter function that must return a boolean value, or a string, symbol, or number, in which case filtering is completed with `(λ (x) (equal? x f))`. The returned value is a list-of-lists wherein the values in column *index* are #t according to *f*.

Examples:

```racket
;;; Sleep data involving the effect of two drugs on 10 
;;; participant's sleep. Data taken from:
;;; Cushny, A. R. and Peebles, A. R. (1905) The action of optical
;;;    isomers: II hyoscines.  The Journal of Physiology 32, 501-510.
(define sleep '((extra group ID)
                (0.7   1     1)
                (-1.6  1     2)
                (-0.2  1     3)
                (-1.2  1     4)
                (-0.1  1     5)
                (3.4   1     6)
                (3.7   1     7)
                (0.8   1     8)
                (0.0   1     9)
                (2.0   1    10)
                (1.9   2     1)
                (0.8   2     2)
                (1.1   2     3)
                (0.1   2     4)
                (-0.1  2     5)
                (4.4   2     6)
                (5.5   2     7)
                (1.6   2     8)
                (4.6   2     9)
                (3.4   2    10)))
				
;;; If we only want to look at data from group 1 (drug #1), we can 
;;; subset that portion of the data set out.
(subset sleep 'group 1)
;;; Equivalently: (subset sleep 'group (λ (x) (equal? x 1)))
;; --> '((extra group ID)
         (0.7   1     1)
         (-1.6  1     2)
         (-0.2  1     3)
         (-1.2  1     4)
         (-0.1  1     5)
         (3.4   1     6)
         (3.7   1     7)
         (0.8   1     8)
         (0.0   1     9)
         (2.0   1    10))
		 
;;; Combined with the `$` function, we can easily calculate the
;;; mean change in sleep for cond 1
(mean ($ (subset sleep 'group 1) 'extra))
;;; Equivalently: (mean ($ (subset sleep 'group (λ (x) (equal? x 1))) 'extra))
;; --> 0.75

;;; Of course, for common practices such as the above example,
;;; consider using the `aggregate` function:
(aggregate mean ($ sleep 'group) ($ sleep 'extra))
;; --> '((1 0.75) (2 2.33))
```


## General Utilities

### log-base (Logarithms with arbitrary base)

```racket
(log-base n #:base [base (exp 1)])
```

Returns the logarithm of `n`. The base can be set with `#:base`

Examples:

```racket
;;; Same as the log function from racket/base
(log-base 10)
;;; --> 2.302585092994046

;;; Compute a base-10 logarithm
(log-base 10 #:base 10)
;;; --> 1.0

```

### Singular Value Decomposition (1-Dimensional)

```racket
(svd-1d A [epsilon 1e-10])
```

Estimates the 1-dimensional singular value decomposition for matrix *A*, stopping when the magnitude between the current and previous estimates (i.e., the cosine of the angle between them) is close to 1 (1 - *epsilon*). `svd-1d` uses the "Power Method" and so can take a long time to converge when the ratio between singular values is close to 1. 

Example:

```racket
;;; Some data
(define my-matrix (matrix [[2 5 3] 
                           [1 2 1] 
                           [4 1 1] 
                           [3 5 2] 
                           [5 3 1] 
                           [4 5 5] 
                           [2 4 2] 
                           [2 2 5]]))

(svd-1d my-matrix)
;; --> (array #[#[-0.5418477730068635] #[-0.6707099886151974] #[-0.5065067640805049]])
```

### z-transform data (Scale)

```racket
(scale lst)
```

Scales (via z-transformation) the data in `lst` such that `(mean lst) --> 0` and `(stddev lst) --> 1`

Example:

```racket 
(scale '(6 2 4 19 3 6))

;; --> '(-0.1168412475673972
;;       -0.8178887329717804
;;       -0.4673649902695888
;;       2.1615630799968484
;;       -0.6426268616206846
;;       -0.1168412475673972)
```

## Statistical Tests and Models
A host of statistical tests and models will be supported, including things such as multiple linear regression, t tests, chi-squared tests, ANOVAs, etc.

### Linear Model

``` racket
(linear-model xs ys)
```
``` racket
(linear-model* xs ys)
```

Estimates simple and multiple linear regression for independent variable(s) *xs* and dependent variable *y*.  For simple linear regression, *xs* should be a single list of observations. For multiple regression, *xs* should be a list-of-lists containing independent variables arranged by "columns". Observed values of the dependent variable should be passed as *y*. 

`linear-model` returns a list containing `'(intercept coefficient ...)`, with one coefficient for every independent (predictor) variable in *xs*.

`linear-model*` returns a hash with the following fields
- **X** --> Design matrix 
- **Y** --> Response vector
- **coef** --> a list containing `'(intercept coefficient ...)`, with one coefficient for every independent (predictor) variable in *xs*
- **residuals** --> (Y-Xβ) as a list
- **n** --> sample size
- **p** --> number of predictors
- **mse** --> mean squared error
- **root-mse** --> root mean squared error

Example: Simple Linear Regression

``` racket
(require math)

;;; Independent variable
(define xs (range 100))

;;; Generate some noisy dependent variable data with a given
;;; intercept and slope
(define intercept 4.83)
(define slope 1.27)
(define ys (map + (map (λ (x) (+ (* x slope) intercept)) xs)
			      (sample (normal-dist 0 30) 100)))


;;; Try to recover the original coefficients (intercept slope)
(linear-model xs ys)

;; Results are pretty close given the noise
;; --> '(0.3148930686704432 1.2717819785002806)

;;; Plot the raw data and the model
(require plot)
(let* ([coef (linear-model xs ys)]
       [slope (cadr coef)]
       [intercept (car coef)])
  (plot (list (points (map vector xs ys))
	      (function (λ (x) (+ (* x slope) intercept))))))
```

![Model Fit](https://github.com/n3mo/data-science/raw/master/img/regression-example.png)

For a more thorough analysis, it can be more useful to store the full model results as a hash using `linear-model*` and then work from there:

```racket
;;; Using the same data from the previous example

;;; Fit the model and store the full results
(define fit (linear-model* xs ys))

;;; Let's inspect the coefficients
(hash-ref fit 'coef)

;;; Using the `qq-plot` functions, we can inspect the residuals to 
;;; ensure normality
(qq-plot* (hash-ref fit 'residuals))
```

![Model Fit](https://github.com/n3mo/data-science/raw/master/img/residuals-example.png)

Example: Multiple Linear Regression

``` racket
;;; First independent variable
(define x1 '(52 59 67 73 64 74 54 61 65 46 72))
;;; Second independent variable
(define x2 '(173 184 194 211 196 220 188 188 207 167 217))
;;; Dependent variable
(define y '(132 143 153 162 154 168 137 149 159 128 166))
;;; Fit the model
(linear-model (map list x1 x2) y)

;; --> '(89144774/2876185 2477588/2876185 963117/2876185)
```

Example: Multiple Linear Regression With Interactions. Individual predictors represent main effects. The interaction between predictors is their product

``` racket
;;; First independent variable
(define x1 '(52 59 67 73 64 74 54 61 65 46 72))
;;; Second independent variable
(define x2 '(173 184 194 211 196 220 188 188 207 167 217))
;;; The interaction of x1 and x2 is their product
(define interaction (map * x1 x2))
;;; Dependent variable
(define y '(132 143 153 162 154 168 137 149 159 128 166))
;;; Fit the model
(linear-model (map list x1 x2 interaction) y)

;; --> '(1054612228677/92932149757 106305446023/92932149757 41389810584/92932149757 -295039649/185864299514)

;;; Let's see the approximate decimal values
(map exact->inexact (linear-model (map list x1 x2 interaction) y))

;; --> '(11.348195769005791 1.1439038728897226 0.4453766612762809 -0.0015873927901779573)

```

### chi-square-goodness

```racket
(chi-square-goodness lst p #:alpha [alpha 0.05])
```

Chi-square goodness of fit test. `lst` should contain variables and observered frequencies as a list of lists. `p` should be a list of hypothesized probabilities, one for each variable in `lst`. The alpha level (default 0.05) can be optionally set.

Example:

```racket
;;; Some count data
(define counts '(("yes" 2700) ("no" 2793)))

;;; For this test, we assume equal probabilities (i.e., we expect
;;; equal "yes" vs "no" responses.
(chi-square-goodness counts '(0.5 0.5))
;;; --> '#hash(('df . 1)
;;;            ('criterion . 3.8414588206941254)
;;;            ('result . "not-significant")
;;;            ('alpha . 0.05)
;;;            ('chisqr . 1.5745494265428728))
```

## Text Processing

### remove-urls

```racket
(remove-urls str)
```

Returns a copy string `str` with URLs removed.

Examples: 

```racket
(remove-urls "This works for regular http://www.example.com and secure urls https://www.example.com")
;;; --> "This works for regular   and secure urls  "
```

### remove-punctuation

```racket
(remove-punctuation str #:websafe? [websafe? #f])
```

Returns a copy of string `str` with punctuation removed. If websafe? is #t, meaningful web punctuation such as #hashtags and @usernames are untouched.

Examples:

```racket
(remove-punctuation "hey, this sentence-which is great-has too; much punctuation!.?")
;;; --> "hey  this sentence which is great has too  much punctuation   "

;;; Clean up punctuation without removing hashtag and username info
(remove-punctuation "clean, this up... but keep #hashtag1 and #hashtag2, along with the @user name!" #:websafe? #t)
;; --> "clean this up but keep #hashtag1 and #hashtag2 along with the @user name"
```

### remove-stopwords

```racket
(remove-stopwords lst #:lexicon [lexicon 'SMART])
```

Returns a copy of list `lst`, as returned by `document->tokens,` with stopwords removed. `#:lexicon` can be the symbols:

- SMART --> Uses the SMART stop-word lexicon
- snowball --> Uses the snowball stop-word lexicon
- onix --> Uses the onix stop-word lexicon

Examples:

```racket
;;; Default lexicon (SMART)
(define text "this is a story about technology and the negative effects that technology can have on our world")
(define words (document->tokens text #:sort? #t))
(remove-stopwords words)
;;; --> '(("technology" 2) ("story" 1) ("negative" 1) ("effects" 1) ("world" 1))

;;; Using a different lexicon
(remove-stopwords words #:lexicon 'snowball)
;;; --> '(("technology" 2)
;;;       ("story" 1)
;;;       ("negative" 1)
;;;       ("effects" 1)
;;;       ("can" 1)
;;;       ("world" 1))

```

### n-gram

```racket
(n-gram str n)
```

Returns a list of all possible n-grams of size `n` (for any `n` greater than zero) in string `str`. If `n` is greater than `(string-length str)` then '() is returned.

Examples:

```racket
;;; Return all unigrams from a string
(n-gram "what a time to be alive" 1)
;;; --> '(("what") ("a") ("time") ("to") ("be") ("alive"))

;;; Return all bigrams from a string
(n-gram "what a time to be alive" 2)
;;; --> '(("what" "a") ("a" "time") ("time" "to") ("to" "be") ("be" "alive"))

;;; Return all trigrams from a string
(n-gram "what a time to be alive" 3)
;;; --> '(("what" "a" "time")
;;;       ("a" "time" "to")
;;;       ("time" "to" "be")
;;;       ("to" "be" "alive"))

;;; etc. for all n-grams
```

### document->tokens

```racket
(document->tokens str [#:sort? #f])
```

For string `str`, returns a list of pairs. Each pair consists of a unique word/token from `str` with its frequency.

Examples:

```racket
(document->tokens "there there are are two two of of everything everything except except this") 
;;; --> '(("there" 2)
;;;       ("are" 2)
;;;       ("two" 2)
;;;       ("of" 2)
;;;       ("everything" 2)
;;;       ("except" 2)
;;;       ("this" 1))
```

### dtm

```racket
(dtm . corpus)
```

Calculates a document-term-matrix for the text in `corpus` (wherein matrix rows correspond to documents and columns to individual words/tokens). More specifically, the **term frequency-inverse document frequency** (tf-idf) matrix is returned. `corpus` should be one or more values as returned by `document->tokens`. The racket/math matrix is returned in a list along with a list of words/tokens found across all documents. The order of the list of tokens corresponds to the columns in the returned dtm. This is the same as the `tdm` function--the returned matrix is simply the transpose of that returned by `tdm`.

Examples:

```racket
;;; Two simple documents
(define doc1 "this is a a sample")
(define doc2 "this is another another example example example")

;;; Convert each document to a list of term frequencies, passing both
;;; to dtm
(dtm (document->tokens doc1)
     (document->tokens doc2))
;; (list
;;  '("sample" "this" "is" "a" "another" "example")
;;  (array
;;   #[#[0.06020599913279623 0 0 0.12041199826559246 0 0]
;;     #[0 0 0 0 0.0860085701897089 0.12901285528456333]]))
```
### tdm

```racket
(tdm . corpus)
```

Calculates a term-document-matrix for the text in `corpus` (wherein matrix rows correspond to words/tokens and columns to individual documents). More specifically, the **term frequency-inverse document frequency** (tf-idf) matrix is returned. `corpus` should be one or more values as returned by `document->tokens`. The racket/math matrix is returned in a list along with a list of words/tokens found across all documents. The order of the list of tokens corresponds to the rows in the returned tdm. This is the same as the `dtm` function--the returned matrix is simply the transpose of that returned by `dtm`.

Examples:

```racket
;;; Two simple documents
(define doc1 "this is a a sample")
(define doc2 "this is another another example example example")

;;; Convert each document to a list of term frequencies, passing both
;;; to tdm
(tdm (document->tokens doc1)
     (document->tokens doc2))
;; (list
;;  '("sample" "this" "is" "a" "another" "example")
;;  (array
;;   #[#[0.06020599913279623 0]
;;     #[0 0]
;;     #[0 0]
;;     #[0.12041199826559246 0]
;;     #[0 0.0860085701897089]
;;     #[0 0.12901285528456333]]))
```

### cosine-similarity

```racket
(cosine-similarity v1 v2)
```

Returns the cosine similarity for matrices v1 and v2.

Example:

```racket
;;; Create two vectors
(define v1 (row-matrix [5 0 3 0 2 0 0 2 0 0]))
(define v2 (row-matrix [3 0 2 0 1 1 0 1 0 1]))

;;; Similarity
(cosine-similarity v1 v2)
;; --> 0.9356014857063997
```

## Sentiment Analysis

Sentiment analysis is commonly used to quickly determine the mood, or emotional valence of a body of text. The `data-science` package offers sentiment analysis via three different lexicons

- nrc lexicon
- bing lexicon
- AFINN lexicon

Individual functions, documented below, offer fine-grained control over analysis options. The following analysis provides an example workflow for accomplishing a sentiment analysis with this package.

```racket
;;; We'll use Racket's net/url package to obtain our text,
;;; data-science to process the text, and plot to visualize the
;;; results 
(require net/url)
(require data-science)
(require plot)
(require math)

;;; We'll use the text of Moby Dick, available for free
;;; on Project Gutenberg
(define moby (string->url "http://www.gutenberg.org/files/2701/2701.txt"))

;;; Open a connection port to the URL
(define in (get-pure-port moby #:redirections 5))

;;; Next, we capture the text from our input port, removing capitalization, 
;;; punctuation, and then extra spaces
(define moby-text (string-normalize-spaces
		   (remove-punctuation
		    (string-downcase (port->string in)) #:websafe? #t)))
			 
;;; Close the input port
(close-input-port in)

;;; To begin our sentiment analysis, we extract each unique word
;;; and the number of times it occurred in the document
(define words (document->tokens moby-text #:sort? #t))

;;; Using the nrc lexicon, we can label each (non stop-word) with an
;;; emotional label. 
(define sentiment (list->sentiment words #:lexicon 'nrc))

;;; We can take a sneak peak at the data...
(take sentiment 5)
;;; --> '(("word" "sentiment" "freq")
;;;       ("ship" "anticipation" 367)
;;;       ("sea" "positive" 364)
;;;       ("time" "anticipation" 318)
;;;       ("long" "anticipation" 311))

;;; sentiment, created above, consists of a list of triplets of the pattern
;;; (token sentiment freq) for each token in the document. Many words will have 
;;; the same sentiment label, so we aggregrate (by summing) across such tokens.
(aggregate sum ($ sentiment 'sentiment) ($ sentiment 'freq))
;;; --> '(("anticipation" 4739)
;;;       ("positive" 9206)
;;;       ("joy" 3196)
;;;       ("trust" 5095)
;;;       ("surprise" 2157)
;;;       ("negative" 7090)
;;;       ("fear" 4136)
;;;       ("sadness" 3317)
;;;       ("anger" 2765)
;;;       ("disgust" 1958))

;;; Better yet, we can visualize this result as a barplot (discrete-histogram)
(let ([counts (aggregate sum ($ sentiment 'sentiment) ($ sentiment 'freq))])
  (parameterize ((plot-width 800))
    (plot (list
	   (tick-grid)
	   (discrete-histogram
	    (sort counts (λ (x y) (> (second x) (second y))))
	    #:color "MediumSlateBlue"
	    #:line-color "MediumSlateBlue"))
	  #:x-label "Affective Label"
	  #:y-label "Frequency")))
```

![Affective Sentiment Labels](https://github.com/n3mo/data-science/raw/master/img/sentiment-labels.png)

```racket
;;; Or, use the bing lexicon to determine the ratio of
;;; positive-to-negative words 
(define sentiment (list->sentiment words #:lexicon 'bing))
(parameterize ([plot-height 200])
  (plot (discrete-histogram
	 (aggregate sum ($ sentiment 'sentiment) ($ sentiment 'freq))
	 #:y-min 0
	 #:y-max 8000
	 #:invert? #t
	 #:color "MediumOrchid"
	 #:line-color "MediumOrchid")
	#:x-label "Frequency"
	#:y-label "Sentiment Polarity"))
```

![Sentiment Polarity](https://github.com/n3mo/data-science/raw/master/img/sentiment-positivity.png)

```racket
;;; It seems that the text is slightly more negative than positive. We
;;; test this using a chi-square goodness of fit test
(let ([counts (aggregate sum ($ sentiment 'sentiment) ($ sentiment 'freq))])
  (chi-square-goodness counts '(.5 .5)))
;;; --> '#hash(('result . "significant")
;;;            ('chisqr . 49.40294802172227)
;;;            ('df . 1)
;;;            ('criterion . 3.8414588206941254)
;;;            ('alpha . 0.05))
;;;
;;; It does seem to be a significant difference
```

```racket
;;; We can also look at which words are contributing the most to our
;;; positive and negative sentiment scores. We'll look at the top 15
;;; influential (i.e., most frequent) positive and negative words
(define negative-tokens
  (take (cdr (subset sentiment 'sentiment "negative")) 15))
(define positive-tokens
  (take (cdr (subset sentiment 'sentiment "positive")) 15))

;;; Some clever reshaping for plotting purposes
(define n (map (λ (x) (list (first x) (- 0 (third x))))
	       negative-tokens))
(define p (sort (map (λ (x) (list (first x) (third x)))
		     positive-tokens)
		(λ (x y) (< (second x) (second y)))))

;;; Plot the results
(parameterize ((plot-width 800)
	       (plot-x-tick-label-anchor 'right)
	       (plot-x-tick-label-angle 90))
  (plot (list
	 (tick-grid)
	 (discrete-histogram n #:y-min -120
			     #:y-max 655
			     #:color "OrangeRed"
			     #:line-color "OrangeRed"
			     #:label "Negative Sentiment") 
	 (discrete-histogram p #:y-min -116
			     #:y-max 649
			     #:x-min 15
			     #:color "LightSeaGreen"
			     #:line-color "LightSeaGreen"
			     #:label "Positive Sentiment"))
	#:x-label "Word"
	#:y-label "Contribution to sentiment"))
	
;;; It seems that the word "like" is contributing extensively to the
;;; positive sentiment. A more thorough analysis might
;;; remove this word given its more common use as simile. 
```

![Word Contributions](https://github.com/n3mo/data-science/raw/master/img/sentiment-contributions.png)

```racket
;;; Or, use the AFINN lexicon to determine the document's
;;; affective polarity
(define sentiment (list->sentiment words #:lexicon 'AFINN))
(sum (map (λ (x) (* (second x) (third x))) (cdr sentiment)))
;;; --> 1220
```

### token->sentiment

```racket
(token->sentiment token [#:lexicon 'nrc])
```

For a given word/token `token`, returns its corresponding sentiment value if possible. `#:lexicon` can be the symbols:

- nrc --> Returns an affective label from the set (anger anticipation disgust fear negative positive sadness surprise trust), or '() if `token` is unknown.
- bing --> Returns a polarity label from the set (negative positive), or '() if `token` is unknown.
- AFINN --> Returns a sentiment score ranging from -4 (very negative) to +4 (very positive), or 0 if `token` is unknown.

Examples:

```racket
;;; nrc lexicon examples --------------------

(token->sentiment "marriage" #:lexicon 'nrc)
;;; --> '(("marriage" "anticipation")
;;;       ("marriage" "joy")
;;;       ("marriage" "positive")
;;;       ("marriage" "trust"))

(token->sentiment "fire" #:lexicon 'nrc)
;;; --> '(("fire" "fear"))

(token->sentiment "the" #:lexicon 'nrc)
;;; --> '()

;;; bing lexicon examples --------------------

(token->sentiment "love" #:lexicon 'bing)
;;; --> '(("love" "positive"))

(token->sentiment "hate" #:lexicon 'bing)
;;; --> '(("hate" "negative"))

(token->sentiment "the" #:lexicon 'bing)
;;; --> '()

;;; AFINN lexicon examples --------------------

(token->sentiment "love" #:lexicon 'AFINN)
;;; --> '(("love" 3))

(token->sentiment "cry" #:lexicon 'AFINN)
;;; --> '(("cry" -1))

(token->sentiment "everest" #:lexicon 'AFINN)
;;; --> '(("everest" 0))
```

### list->sentiment

```racket
(list->sentiment lst [#:lexicon 'nrc])
```

Returns a list of sentiment values for `lst`, which is a list of pairs of words/tokens and their frequency, as returned by `document->tokens`. Returns a list of triplets of the form (token sentiment frequency). `#:lexicon` can be the symbols:

- nrc --> Returns an affective label from the set (anger anticipation disgust fear negative positive sadness surprise trust), or '() if `token` is unknown.
- bing --> Returns a polarity label from the set (negative positive), or '() if `token` is unknown.
- AFINN --> Returns a sentiment score ranging from -4 (very negative) to +4 (very positive), or 0 if `token` is unknown.

Examples:

```racket
;;; Turn a document into word frequencies
(define tokens (document->tokens "happy happy happy love is better than ugly ugly mean old hate"))
;;; --> '(("happy" 3)
;;;       ("love" 1)
;;;       ("is" 1)
;;;       ("better" 1)
;;;       ("than" 1)
;;;       ("ugly" 2)
;;;       ("mean" 1)
;;;       ("old" 1)
;;;       ("hate" 1))

;;; Convert the tokens into sentiment scores using the nrc lexicon
(list->sentiment tokens #:lexicon 'nrc)
;;; --> '(("word" "sentiment" "freq")
;;;       ("happy" "anticipation" 3)
;;;       ("happy" "joy" 3)
;;;       ("happy" "positive" 3)
;;;       ("happy" "trust" 3)
;;;       ("love" "joy" 1)
;;;       ("love" "positive" 1)
;;;       ("ugly" "disgust" 2)
;;;       ("ugly" "negative" 2)
;;;       ("hate" "anger" 1)
;;;       ("hate" "disgust" 1)
;;;       ("hate" "fear" 1)
;;;       ("hate" "negative" 1)
;;;       ("hate" "sadness" 1))

;;; Convert the tokens into sentiment scores using the bing lexicon
(list->sentiment tokens #:lexicon 'bing)
;;; --> '(("word" "sentiment" "freq")
;;;      ("happy" "positive" 3)
;;;      ("love" "positive" 1)
;;;      ("better" "positive" 1)
;;;      ("ugly" "negative" 2)
;;;      ("hate" "negative" 1))

;;; Convert the tokens into sentiment scores using the AFINN lexicon
(list->sentiment tokens #:lexicon 'AFINN)
;;; --> '(("word" "sentiment" "freq")
;;;       ("happy" 3 3)
;;;       ("love" 3 1)
;;;       ("is" 0 1)
;;;       ("better" 2 1)
;;;       ("than" 0 1)
;;;       ("ugly" -3 2)
;;;       ("mean" 0 1)
;;;       ("old" 0 1)
;;;       ("hate" -3 1))
```

## Plotting Functions

### Histogram of Sorted Counts

```racket
(hist lst)
```
```racket
(hist* lst)
```

Generating discrete histograms of sorted observed frequencies in a sample requires several unnecessarily unwieldy steps. This should be easier. Now it is. `hist` returns a renderer that produces a sorted, discrete histogram of observed frequencies. `hist*` plots the renderer for you for convenience. Both functions use `sorted-counts` to sort and group the data.

Examples:

```racket
;;; If we roll two dice in a board game, what are the most
;;; frequent totals? Let's simulate rolling 500 pairs of dice
(define die-1 (build-list 500 (λ (n) (random 1 7))))
(define die-2 (build-list 500 (λ (n) (random 1 7))))
(define total-roll (map + die-1 die-2))

;;; Aha... this is why we get robbed so often in Settlers of Catan!
(hist* total-roll)
```

![Hist Plot](https://github.com/n3mo/data-science/raw/master/img/hist-example.png)

```racket
;;; We can generate the same hist figure above manually, exercising
;;; some control over the plot details. Let's make an ugly green histogram 
(parameterize ([rectangle-color "lightgreen"]) 
  (plot (hist total-roll)
        #:x-label "Dice Roll"
        #:y-label "Frequency"))
```

### Quantile-Quantile (Q-Q) Plot

```racket
(qq-plot lst [#:scale? #t])
```
```racket
(qq-plot* lst [#:scale? #t])
```

Plots sample quantiles against theoretical quantiles from a normal distribution with a mean and standard deviation of the sample `lst`. By default, both sequences of quantiles are z-transformed. Suppress this behavior with #:scale? #f. `qq-plot` returns a renderer to be used with `plot`, `plot-file`, etc. `qq-plot*`  produces a plotted figure for quick convenience. 

Examples:

```racket
;;; Normal data should plot close to y = x
(qq-plot* (sample (normal-dist 0 1) 500))
```

![Q-Q Plot](https://github.com/n3mo/data-science/raw/master/img/qq-plot-example.png)

```racket
;;; Non-normal data is visibly non-linear. These data are drawn
;;; from a right-skewed gamma distribution
(qq-plot* (sample (gamma-dist 2 2) 500))
```

![Q-Q Skewed Plot](https://github.com/n3mo/data-science/raw/master/img/qq-plot-skewed-example.png)

```racket
;;; Use qq-plot to return a renderer. With this, you can exercise 
;;; manual control with the workflow typical of the plot package
(parameterize ([point-size 10]
			   [point-color "blue"])
  (plot (list (qq-plot (sample (gamma-dist 2 2) 500))
              (function identity #:color "red" #:width 2))
        #:x-label "Theoretical Quantiles"
        #:y-label "Sample Quantiles (n = 500)"))
```

![Q-Q Manual Plot](https://github.com/n3mo/data-science/raw/master/img/qq-manual-example.png)

# Bugs & Improvements

Please report any problems that you find, along with any suggestions or contributions. 

You can support this project, or my other projects via [ChangeTip](http://n3mo.tip.me)

[![Support via ChangeTip](http://www.nicholasvanhorn.com/images/changetip.png)](http://n3mo.tip.me)


# License

Copyright (C) 2016 Nicholas M. Van Horn

Author: Nicholas M. Van Horn <nvanhorn@capital.edu>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
