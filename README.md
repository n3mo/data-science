# Data Science Tooling For Racket

This project is a work in progress and is not currently intended to be shared with others. You are, however, welcome to snoop around. Eventually, this will evolve into a package of convenience functions for data science: data cleaning, wrangling, summarizing, and exploration. 

Many functions contained within are inspired by functionality commonly available in more statistically-oriented software packages such as R, numpy/scipy, and Matlab/Octave. This all began because I wanted the R function `aggregate` in Racket.

## Installation
The data-science package is not currently registered in Racket's package catalog. It can, however, be installed directly from GitHub via raco:

``` racket
raco pkg install https://github.com/n3mo/data-science.git
```

# Usage

## Split->Apply->Combine
`data-science` provides a collection of ultility functions for breaking your data into meaningful pieces, applying functions to each piece, and then recombining the results. In fact, the filter/map/apply approach of lisp-like languages is well suited to such tasks. However, with complex analyses, commands can grow quite complex and cumbersome, and can mask their intended purpose. The following functions provide convenient short-hand procedures that aim to be *expressive, yet concise*.

### $

``` racket
($ lst index)
```

Returns a "column" of data from a list-of-lists *lst*. Column selection is controlled by *index,* which can be either a number or a symbol. If *index* is a number, the corresponding column is returned from all rows of *lst*. If *index* is a symbol, then the first row of *lst* is assumed to be a header containing named fields of each column. In this situation, $ returns the corresponding column of data identified by the column name, *excluding the first row*--that is, the header name is **not** part of the return value.

Examples:

``` racket
;;; Numerical indexing
> (define my-data '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))
> ($ my-data 0)
'(1 4 7 10)

;;; Indexing by name
> (define my-data '((age rank id) (21 1 100) (18 2 101) (32 1 102) (19 4 103)))
> ($ my-data 'rank)
'(1 2 1 4)
```

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
