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
	cores = 15
)
# ------- Make graphics
dnart_project %>%
	plotArt(age = 2) 
dnart_project %>%
	plotArt(age = 10) %>% 
	plotArt(age = 20)
```

## Plot in shapes 

```r
dnart_project %>%
	addLineShape() %>% 
		plotArt(age = 30) %>% 
		plotArt(age = 40) %>% 
		plotArt(age = 50) %>% 
		plotArt(age = 60) %>% 
		plotArt(age = 70) %>% 
	addLoessShape() %>% 
		plotArt(age = 30) %>% 
		plotArt(age = 40) %>% 
		plotArt(age = 50) %>% 
		plotArt(age = 60) %>% 
		plotArt(age = 70) %>% 
	addRingShape() %>% 
		plotArt(age = 30) %>% 
		plotArt(age = 40) %>% 
		plotArt(age = 50) %>% 
		plotArt(age = 60) %>% 
		plotArt(age = 70) 
```

## Using custom palettes

```r
# ---- Palette from scico package
dnart_project %>%
	addRingShape() %>% 
	plotArt(age = 50, palette = scico::scale_fill_scico(palette = 'davos'))

# ---- Palette from Rcolorbrewer
pal = "RdYlBu"
plotArt(
	getRingRadius(dnart_project), 
	age = 50, 
	palette = scale_fill_distiller(palette = pal), 
	pdf = glue::glue('plot_palette.', pal, '.pdf')
)

# ---- Palette generated from 6 main colors in an image
cols <- getPaletteFromImg("data/PK03T1.jpg", ncols = 20)
colplot <- checkPalette(cols)
plotArt(
	getRingRadius(dnart_project), 
	age = 90, 
	palette = scale_fill_gradientn(colors = cols[c(1:4, 11, 17, )]), 
	pdf = glue::glue('plot_palette.', 'gradient', '.pdf')
)

# ---- Custom palette from discrete colors
cols <- c('#ebebeb', '#e3e3e3', '#c9c9c9', '#636363', '#575757', '#4a4949')
plotArt(
	getRingRadius(dnart_project), 
	age = 90, 
	palette = scale_fill_gradientn(colors = cols), 
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
