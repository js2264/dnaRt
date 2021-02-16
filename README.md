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

This is a basic example which shows you how to generate dn&art graphics. 
I used my "variables" to define the project:

```r
library(dnaRt)
# ------- Initiate project
dnart_project <- dnart(
	dob = "20/06/1992", 
	given = "jacques", 
    top = 100, 
	folder = 'data', 
	step = 5, 
	width = 100, 
	cores = 15
)
# ------- Make graphics
dnart_project %>% 
	getRingRadius() %>% 
	plotArt(age = 2, palette = scico::scale_fill_scico(palette = 'roma')) %>% 
	plotArt(age = 5, palette = scico::scale_fill_scico(palette = 'roma')) %>% 
	plotArt(age = 10, palette = scico::scale_fill_scico(palette = 'roma')) %>% 
	plotArt(age = 20, palette = scico::scale_fill_scico(palette = 'roma')) %>% 
	plotArt(age = 30, palette = scico::scale_fill_scico(palette = 'roma')) %>% 
	plotArt(age = 40, palette = scico::scale_fill_scico(palette = 'roma')) %>% 
	plotArt(age = 50, palette = scico::scale_fill_scico(palette = 'roma')) %>% 
	plotArt(age = 60, palette = scico::scale_fill_scico(palette = 'roma')) %>% 
	plotArt(age = 70, palette = scico::scale_fill_scico(palette = 'roma')) %>% 
	plotArt(age = 80, palette = scico::scale_fill_scico(palette = 'roma')) %>% 
	plotArt(age = 90, palette = scico::scale_fill_scico(palette = 'roma')) %>% 
	plotArt(age = 99, palette = scico::scale_fill_scico(palette = 'roma'))
```
