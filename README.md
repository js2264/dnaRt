# dnaRt

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![pkgdown](https://github.com/js2264/dnaRt/workflows/pkgdown/badge.svg)](https://github.com/js2264/dnaRt/actions)
<!-- badges: end -->

## Installation

You can install the released version of dnaRt from [Github](https://github.com/js2264/dnaRt) with:

``` r
remote::install_github("js2264/dnaRt")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(tidyverse)
library(dnaRt)
yob <- "1991"
dob <- "0826"
name <- 'maud'
project <- initiateProject(yob, dob, name) %>% 
    getSequences(top = 40) %>% 
    getPairwiseAlnScores(cores = 14) %>% 
    getGraph() %>% 
    getLayout() %>% 
    getPlottingData()
plotArt(project, age = 20)
```

