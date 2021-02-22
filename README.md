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

## dnArt

### Quick use

This is a basic example which shows you how to generate dn&art graphics. 
I used my "variables" to define the project:

```r
library(dnaRt)
# ------- Initiate project
dnart_project <- dnart(
	folder = 'data',
	given = "jacques", 
	dob = "20/06/1992", 
    top = 100
)
# ------- Make default, full art
plotArt(dnart_project)
```

### Plot with shapes

Plots can be piped with `magrittr`:

`filterBy()` is used to specify which value to use to filter out points: 
- By default, all the points are plotted
- If a shape is added, `filterBy('dist_shape')` should be used (if a full disc shape is wanted, don't use `filterBy`)
- If an age or a date is entered in `plotArt`, then points exceeding this date are further removed

```r
library(magrittr)
# ------- Make several graphics with options (shape, filtering, both)
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

### Simulated plots 

```r
# ------ Simulate a single project
proj <- randomProject()
plotArt(proj)
# ------ Simulate several projects
list_projects <- parallel::mclapply(mc.cores = 5, 1:5, function(seed) randomProject(seed = seed, nedges = 'max'))
# ------ Plot several ages for a single plot
plotAges(list_projects[[1]], seq(1, 20, by = 3))
# ------ Plot a single year for multiple plots
list_projects <- lapply(list_projects, plotArt, path = 'results')
```

### Using custom palettes

```r
data(sample_data, package = 'dnaRt')
dnart_project <- dnart(given = 'James', dob = '11/11/1990', data = sample_data)
# ---- Palette from scico package
dnart_project %>%
	addRingShape() %>% 
	addPalette(scico::scale_fill_scico(palette = 'davos')) %>% 
	plotArt(age = 50)
dnart_project %>%
	addRingShape() %>% 
	addPalette(scico::scale_fill_scico(palette = 'vik')) %>% 
	plotArt(age = 51)
# ---- Palette from Rcolorbrewer
pal = "RdYlBu"
dnart_project %>%
	addRingShape() %>% 
	addPalette(scale_fill_distiller(palette = pal)) %>% 
	plotArt(age = 52)
# ---- Palette from Wes Anderson movies
pal = scale_fill_gradientn(colours = wesanderson::wes_palette("Royal1", 5, type = "continuous"))
dnart_project %>%
	addRingShape() %>% 
	addPalette(pal) %>% 
	plotArt(age = 53)
# ---- Palette generated from main colors from an image
img <- "https://miro.medium.com/max/2000/1*QhGyZ9TJDFy_pWwnhCcZqA.png"
cols <- getPaletteFromImg(img, ncols = 20)
colplot <- checkPalette(cols)
dnart_project %>% 
	addRingShape() %>% 
	addPalette(scale_fill_gradientn(colors = cols[c(1, 2, 6, 9, 10, 11, 12, 15, 16)])) %>% 
	plotArt(age = 54)
# ---- Custom palette from discrete colors
cols <- c('#ebebeb', '#e3e3e3', '#c9c9c9', '#636363', '#575757', '#4a4949')
dnart_project %>% 
	addRingShape() %>% 
	addPalette(scale_fill_gradientn(colors = cols)) %>% 
	plotArt(age = 55)
```

### Advanced customization

```r
dnart_project <- dnart(given = 'James', dob = '11/11/1991', data = sample_data)
# ---- Zooming to a portion of the plot 
dnart_project %>%
	plotArt(pdf = 'zoom-factor1.pdf') %>%
	plotArt(zoom = 2, pdf = 'zoom-factor2.pdf') %>%
	plotArt(zoom = 3, pdf = 'zoom-factor3.pdf') %>%
	plotArt(zoom = 4, pdf = 'zoom-factor4.pdf') 
# ---- Rectangle ratio
dnart_project %>% 
	plotArt(ratio = 3/2, pdf = 'ratio-3/2.pdf') %>%
	plotArt(ratio = 2, pdf = 'ratio-2.pdf')
# ---- Adding solid background
dnart_project %>%
	addPalette(scico::scale_fill_scico(palette = 'vik')) %>% 
	plotArt(
		theme.args = theme(plot.background = element_rect(fill = "#fffbf2")),
		pdf = 'solid-background.pdf'
	)
# ---- Background for MPB
plotArt(dnart_project, ratio = 1.6, pdf = 'MBP-background.pdf')
# ------ Plot portrait or landscape
pal = scale_fill_gradientn(colours = wesanderson::wes_palette("FantasticFox1", 5, type = "continuous"))
proj <- list_projects[[1]] %>% 
	addPalette(pal) %>% 
	plotArt(
		path = 'orientations', 
		ratio = 1.66, 
		zoom = 3, 
		orientation = 'portrait', 
		file = 'portrait.pdf'
	)
proj <- list_projects[[1]] %>% 
	addPalette(pal) %>% 
	plotArt(
		path = 'orientations', 
		ratio = 1.66, 
		zoom = 3, 
		orientation = 'landscape', 
		file = 'landscape.pdf'
	)
```

### Generating hundreds of wallpapers 

```r
# ------ Simulate several projects
list_projects <- parallel::mclapply(mc.cores = 5, 51:100, function(seed) randomProject(seed = seed))
list_plots <- parallel::mclapply(mc.cores = 5, 
	seq_along(list_projects), function(K) {
		set.seed(K)
		cols = sample(names(wesanderson::wes_palettes), 1)
		pal = scale_fill_gradientn(colours = wesanderson::wes_palette(cols, 5, type = "continuous"))
		proj <- list_projects[[K]] %>% 
			addPalette(pal) %>% 
			plotArt(
				path = 'backgrounds', 
				ratio = 1, 
				zoom = sample(1:4, 1)
			)
	}
)
```

## Voronoi transformation of pre-existing image

```r
p <- '~/Documents/Admin/__Photo/photo_in_lab_square-01.png' %>% 
	readImg() %>%
	binImg() %>% 
	voronoiTiles(img, density = 20000)
```

