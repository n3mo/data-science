# Data Science Tooling For Racket

This project is a work in progress. Documented functions are working, however, and you are welcome to snoop around. Eventually, this will evolve into a package of convenience functions for data science: data cleaning, wrangling, summarizing, and exploration. 

Many functions contained within are inspired by functionality commonly available in more statistically-oriented software packages such as R, numpy/scipy, and Matlab/Octave. This all began because I wanted the R function `aggregate` in Racket.

# Table of Contents

- [Installation](#installation)
- [Data Import/Export](#importexport)
 - [CSV Reading](#read-csv)
- [Split -> Apply -> Combine Workflows](#split-apply-combine)
 - [Column Indexing](#-column-indexing)
 - [Aggregate](#aggregate)
 - [Grouping Data](#group-with)
 - [Counting Samples](#sorted-counts)
 - [Subsetting Data](#subset)
- [Statistical Utilities](#general-utilities)
 - [Singular Value Decomposition](#singular-value-decomposition-1-dimensional)
 - [Data scaling: Z-transformation](#z-transform-data-scale)
- [Statistical Tests/Models](#statistical-tests-and-models)
 - [Linear Regression Models](#linear-model)
- [Text Processing](#text-processing)
 -[Sentiment Analysis](#sentiment-analysis)
  -[Tokenizing Documents](#document-tokens)
  -[Token/word sentiment](#token-sentiment)
  -[Document Sentiment](#list->sentiment)
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

### Read-CSV

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

Returns (filters) a subset of data from a list-of-lists *lst*. Column selection is controlled by *index,* which can be either a number or a symbol. If *index* is a number, data from the corresponding column is used. If *index* is a symbol, then the first row of *lst* is assumed to be a header containing named fields of each column. In this situation, data from the corresponding column of data identified by the column name is used, *excluding the first row*--that is, the header row is not subjected to filtering, and is returned un-touched as part of the returned subset. In either case, the values from the indexed column are filtered via the function *f*, which is a 1-parameter function that must return a boolean value. The returned value is a list-of-lists wherein the values in column *index* are #t according to *f*.

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
(subset sleep 'group (λ (x) (equal? x 1)))
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
(mean ($ (subset sleep 'group (λ (x) (equal? x 1))) 'extra))
;; --> 0.75

;;; Of course, for common practices such as the above example,
;;; consider using the `aggregate` function:
(aggregate mean ($ sleep 'group) ($ sleep 'extra))
;; --> '((1 0.75) (2 2.33))
```


## General Utilities

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

# Text Processing

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

;;; We'll use the text of Alice in Wonderland, available for free
;;; on Project Gutenberg
(define alice (string->url "https://ia801405.us.archive.org/18/items/alicesadventures19033gut/19033.txt"))

;;; Open a connection port to the URL
(define in (get-pure-port alice #:redirections 5))

;;; Next, we capture the text from our input port, removing capitalization, 
;;; punctuation, and then extra spaces
(define alice-text (string-normalize-spaces
                    (remove-punctuation
                     (string-downcase (port->string in)))))
			 
;;; Close the input port
(close-input-port in)

;;; To begin our sentiment analysis, we extract each unique word
;;; and the number of times it occurred in the document
(define words (document->tokens alice-text #:sort? #t))

;;; Using the nrc lexicon, we can label each (non stop-word) with an
;;; emotional label, counting up each occurrence of each label
(sorted-counts (filter (λ (x) x) (list->sentiment words #:lexicon 'nrc)))

;;; Better yet, we can visualize this result as a barplot (discrete-histogram)
(parameterize ((plot-width 800))
  (plot (list
	 (tick-grid)
	 (discrete-histogram
	  (sorted-counts (filter (λ (x) x) (list->sentiment words #:lexicon 'nrc)))))
	#:x-label "Affective Label"
	#:y-label "Frequency"))
```

![Affective Sentiment Labels](https://github.com/n3mo/data-science/raw/master/img/sentiment-labels.png)

```racket
;;; Or, use the bing lexicon to determine the ratio of
;;; positive-to-negative words 
(plot (discrete-histogram
       (sorted-counts (filter (λ (x) x) (list->sentiment words #:lexicon 'bing))))
      #:x-label "Sentiment Polarity"
      #:y-label "Frequency")
```

![Sentiment Polarity](https://github.com/n3mo/data-science/raw/master/img/sentiment-positivity.png)

```racket
;;; Or, use the AFINN lexicon to determine the document's
;;; affective polarity
(apply + (list->sentiment words #:lexicon 'AFINN))
```

### document->tokens

### token->sentiment

### list->sentiment

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
