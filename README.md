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
	plotArt(age = 2, palette = scico::scale_fill_scico(palette = 'davos')) %>% 
	plotArt(age = 5, palette = scico::scale_fill_scico(palette = 'davos')) %>% 
	plotArt(age = 10, palette = scico::scale_fill_scico(palette = 'davos')) %>% 
	plotArt(age = 20, palette = scico::scale_fill_scico(palette = 'davos')) %>% 
	plotArt(age = 30, palette = scico::scale_fill_scico(palette = 'davos')) %>% 
	plotArt(age = 40, palette = scico::scale_fill_scico(palette = 'davos')) %>% 
	plotArt(age = 50, palette = scico::scale_fill_scico(palette = 'davos')) %>% 
	plotArt(age = 60, palette = scico::scale_fill_scico(palette = 'davos')) %>% 
	plotArt(age = 70, palette = scico::scale_fill_scico(palette = 'davos')) %>% 
	plotArt(age = 80, palette = scico::scale_fill_scico(palette = 'davos')) %>% 
	plotArt(age = 90, palette = scico::scale_fill_scico(palette = 'davos')) %>% 
	plotArt(age = 99, palette = scico::scale_fill_scico(palette = 'davos'))
```

## Using custome palettes

```r
# ---- Palette from scico package
palettes <- scico::scico_palette_names()
for (col in palettes) {
getRingRadius(dnart_project) %>% 
	plotArt(age = 50, palette = scico::scale_fill_scico(palette = col), pdf = glue::glue('plot_palette.', col, '.pdf'))
}

# ---- Palette from Rcolorbrewer
pal = "RdYlBu"
plotArt(
	getRingRadius(dnart_project), 
	age = 50, 
	palette = scale_fill_distiller(palette = pal), 
	pdf = glue::glue('plot_palette.', pal, '.pdf')
)

# ---- Palette generated from 6 main colors in an image
cols <- getPaletteFromImg("data/PK03T1.jpg", ncols = 4)
colplot <- checkPalette(cols)
plotArt(
	getRingRadius(dnart_project), 
	age = 90, 
	palette = scale_fill_gradientn(colors = cols), 
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
