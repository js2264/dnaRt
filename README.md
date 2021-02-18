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

## Quick use

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
	cores = 5
)
# ------- Make default, full art
plotArt(dnart_project)
```

## Plot with shapes

Plots can be piped with `magrittr`:

`filterBy()` is used to specify which value to use to filter out points: 
- By default, all the points are plotted
- If a shape is added, `filterBy('dist_shape')` should be used (if a full disc shape is wanted, don't use `filterBy`)
- If an age or a date is entered in `plotArt`, then points exceeding this date are further removed

```r
library(magrittr)
# ------- Make several graphics with options (shape, filtering, both)
dnart_project <- randomProject(seed = 2002)
dnart_project %>%
	plotArt(age = 2) %>% 
	plotArt(age = 10) 
#
dnart_project %>%
	addRingShape() %>%
	plotArt(age = 2) %>% 
	plotArt(age = 10) %>% 
	filterBy('dist_shape') %>% 
	plotArt(age = 11) %>% 
	plotArt(age = 40) %>%
	plotArt(age = 80) 
#
dnart_project %>%
	addEllipticShape() %>%
	filterBy('dist_shape') %>% 
	plotArt(age = 2) %>% 
	plotArt(age = 10) %>% 
	plotArt(age = 80)
#
dnart_project %>%
	addLoessShape() %>%
	filterBy('dist_shape') %>% 
	plotArt(age = 2) %>% 
	plotArt(age = 10) %>% 
	plotArt(age = 80)
#
dnart_project %>%
	addLineShape() %>%
	filterBy('dist_shape') %>% 
	plotArt(age = 2) %>% 
	plotArt(age = 10) %>% 
	plotArt(age = 80)
```

## Using custom palettes

```r
# ---- Palette from scico package
dnart_project %>%
	addRingShape() %>% 
	addPalette(scico::scale_fill_scico(palette = 'davos')) %>% 
	plotArt(age = 50)
dnart_project %>%
	addRingShape() %>% 
	addPalette(scico::scale_fill_scico(palette = 'vik')) %>% 
	plotArt(age = 50)

# ---- Palette from Rcolorbrewer
pal = "RdYlBu"
dnart_project %>%
	addRingShape() %>% 
	addPalette(scale_fill_distiller(palette = pal)) %>% 
	plotArt(age = 50)

# ---- Palette generated from main colors from an image
img <- "https://miro.medium.com/max/2000/1*QhGyZ9TJDFy_pWwnhCcZqA.png"
cols <- getPaletteFromImg(img, ncols = 20)
colplot <- checkPalette(cols)
dnart_project %>% 
	addRingShape() %>% 
	addPalette(scale_fill_gradientn(colors = cols[c(1, 2, 6, 9, 10, 11, 12, 15, 16)])) %>% 
	plotArt(
		age = 90, 
		pdf = glue::glue('plot_palette.', 'gradient', '.pdf')
	)

# ---- Custom palette from discrete colors
cols <- c('#ebebeb', '#e3e3e3', '#c9c9c9', '#636363', '#575757', '#4a4949')
dnart_project %>% 
	addRingShape() %>% 
	addPalette(scale_fill_gradientn(colors = cols)) %>% 
	plotArt(
		age = 90, 
		pdf = glue::glue('plot_palette.', 'gradient', '.pdf')
	)
```

## Further customizing

```r
# ---- Adding background
cols <- c('#ebebeb', '#e3e3e3', '#c9c9c9', '#636363', '#575757', '#4a4949')
plotArt(
	getRingRadius(dnart_project), 
	age = 90, 
	palette = scale_fill_gradientn(colors = cols), 
	theme.args = theme(plot.background = element_rect(fill = "#000000")),
	pdf = glue::glue('plot_palette.', 'gradient', '.pdf')
)
```
