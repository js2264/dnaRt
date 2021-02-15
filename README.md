# dnaRt

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![pkgdown](https://github.com/js2264/dnaRt/workflows/pkgdown/badge.svg)](https://github.com/js2264/dnaRt/actions)
<!-- badges: end -->

## Installation

You can install the released version of dnaRt from [Github](https://github.com/js2264/dnaRt) with:

``` r
remotes::install_github("js2264/dnaRt")
```

## Example

This is a basic example which shows you how to solve a common problem:

```r
library(dnaRt)
dnart_project <- dnart(
	dob = "20/06/1992", 
	given = "jacques", 
    top = 100, 
	folder = 'data', 
	step = 5, 
	width = 100, 
	force = TRUE, 
	cores = 15
)
plotArt(dnart_project, age = 3)
plotArt(project, date = "29/09/2020")
```

