#' plotArt
#'
#' @param project 
#' @param date 
#' @param age 
#' @param pdf 
#' @param path 
#' @param zoom 
#' @param max.radius
#' @param theme.args 
#'
#' @return project (invisible)
#'
#' @export

plotArt <- function(
    project, 
    date = NULL, 
    age = NULL,  
    zoom = 1, 
    ratio = 1, 
    file = NULL, 
    path = 'output',
    orientation = 'portrait',
    background = FALSE,
    maxsize = 49,
    dpi = 300, 
    max.radius = 0.0075,
    theme.args = NULL, 
    seed = NULL, 
    ...
) {
    `%>%` <- tidyr::`%>%`
    library(ggplot2)
    yob <- project[["yob"]]
    dob <- project[["dob"]]
    given <- project[["given"]]
    folder <- project[["folder"]]
    project_path <- project[["project_path"]]
    top <- project[["top"]]
    # ------- Get seed
    if (is.null(seed)) {
        seed <- digest::digest2int(glue::glue("{yob}{dob}"))
    }
    # ------- Get palette 
    if ("palette" %in% names(project)) {
        if ("Scale" %in% class(project$palette)) {
            palette <- project$palette
        }
        else {
            palette <- scale_fill_manual(values = project$palette)
        }
    }
    else {
        cols <- c("#89B9BD", "#323348", "#A25543", "#FFD3B8", "#149698", "#8A151B","#F04635")
        palette <- scale_fill_gradientn(colors = cols)
    }
    # ------- Get plot_data from project
    if ("plot_data" %in% names(project)) {
        plotdf <- project[["plot_data"]]
        type <- attributes(project[["plot_data"]])$type
    }
    else {
        plotdf <- project[["data"]]
        plotdf$x_final <- plotdf$x
        plotdf$y_final <- plotdf$y
        type <- 'raw'
    }
    # ------- Get step if missing
    if (!'step' %in% colnames(plotdf)) plotdf$step <- factor(plotdf$year)
    # ------- Set the limit for filtering (based on a input date, input age, or no filtering)
    set.seed(seed)
    if (is.null(age) & !is.null(date)) {
        age <- as.numeric(as.Date(date, "%d/%m/%Y") - dob)
        limit <- round(age/365.25)
        date <- as.Date(
            date, 
            format = "%d/%m/%Y"
        )
    }
    else if (!is.null(age) & is.null(date)) {
        limit <- age
        age <- floor(age * 365.25)
        date <- dob + age
    } 
    else if (is.null(age) & is.null(date)) {
        limit <- top
        age <- top
        date <- NA
    } 
    # ------- Stop if queried age is higher than max computed age
    if (limit > top) {
        msg_warning("Age is higher than the maximum computed age. Retry with lower age.")
        stop()
    }
    # ------- Import plot df 
    df_base <- plotdf
    df <- df_base
    # ------- Filter nodes <= year
    df <- df %>% dplyr::filter(as.numeric(step) <= limit) 
    # ------- Make sure that data ranges are rescaled to 0-1
    df[, c('x_final', 'y_final')] <- fitXY(df[, c('x_final', 'y_final')])
    # ------- Inverse ratio if landscape mode
    if (orientation == 'landscape') {
        ratio <- 1/ratio
    }
    # ------- Rescale y's to ratio
    df$y_final <- scales::rescale(df$y_final, c(0, ratio))
    # ------- Add fill aesthetics and colored_tiles
    set.seed(seed)
    K <- round(nrow(df) / {2 - (limit/top)})
    K_rest <- nrow(df) - K
    df <- df %>%
        dplyr::mutate(
            fill = sample(1:age, nrow(.), replace = TRUE)/{3*nrow(.)}, # This picks a random value (continuous) for blacks palette
            colored_tile = sample(c(sample(2:1000, K, replace = TRUE), rep(1, K_rest))), # This picks a random value (continuous) for color palette. The fuller the network is, the more colored it becomes ( K -> nrow(df) )
            colored_tile_alpha = sample(c(sample(2:1000, K, replace = TRUE), rep(1, K_rest))) # For the colored points, gives a transparency
        )
    # ------- Discretize if discrete fill palete
    if ('ScaleDiscrete' %in% class(palette)) {
        df <- df %>%
            dplyr::mutate(
                fill = 0, # No blacks palette
                colored_tile = factor(sample(1:length(project$palette), nrow(.), replace = TRUE)), # Pick colors from palette range
                colored_tile_alpha = 1 # For the colored points, gives a transparency
            )
    }
    # ------- Add background
    if (background) {
        baseplot <- backgroundTiles()
    } 
    else {
        baseplot <- ggplot(df)
    }
    p <- baseplot
    # ------- Add black inner circles 
    if (!'ScaleDiscrete' %in% class(palette)) {
        blacks <- scale_fill_gradient(low = 'white', high = '#000000')
        p <- p + 
            ggforce::geom_voronoi_tile(
                data = df, aes(x = x_final, y = y_final, fill = fill, group = -1L), 
                alpha = 0.6,
                expand = unit(-.55, 'mm'), 
                radius = unit(0.25, 'mm'), 
                max.radius = max.radius, 
                normalize = TRUE, 
                asp.ratio = 1/ratio
            ) + blacks + ggnewscale::new_scale_fill()
    }
    # ------- Add colored circles
    p <- p + 
        ggforce::geom_voronoi_tile(
            data = df, aes(x = x_final, y = y_final, fill = colored_tile, alpha = colored_tile_alpha, group = -1L), 
            expand = unit(-.35, 'mm'), 
            radius = unit(0.25, 'mm'), 
            max.radius = max.radius*1.25, 
            normalize = TRUE, 
            asp.ratio = 1/ratio
        ) + palette 
    if ('ScaleDiscrete' %in% class(palette)) {
        p <- p + scale_alpha(range = c(0.99, 1))
    }
    # ------- Add theme arguments
    p <- p + 
        theme_void() + 
        theme(legend.position = 'none') 
    if (!is.null(theme.args)) p <- p + theme.args
    # ------- Zoom & frame the plot
    p <- zoomify(p, zoom = zoom, ratio = ratio, seed = seed)
    # ------- Save plot
    if (is.null(file)) {
        if (is.null(path)) {
            plot_path <- glue::glue("{project_path}/plots/plot_{given}_{dob}_{type}_{date}.pdf")
        }
        else {
            plot_path <- glue::glue("{path}/plot_{given}_{dob}_{type}_{date}.pdf")
        }
    }
    else {
        plot_path <- file
    }
    if (!dir.exists(dirname(plot_path))) 
        dir.create(dirname(plot_path), showWarnings = FALSE)
    ggsave(plot = p, plot_path, width = maxsize/ratio, height = maxsize, dpi = dpi, units = 'in', limitsize = FALSE)
    msg_success(glue::glue("Plot saved in {plot_path}"))
    # ------- Return project
    project$plot_final <- df
    project$plot <- p
    invisible(project)
}

plotArt_2 <- function(
    project, 
    date = NULL, 
    age = NULL,  
    zoom = 1, 
    ratio = 1, 
    file = NULL, 
    path = 'output',
    orientation = 'portrait',
    background = FALSE,
    maxsize = 49,
    dpi = 300, 
    max.radius = 0.02,
    theme.args = NULL, 
    seed = NULL, 
    ...
) {
    `%>%` <- tidyr::`%>%`
    library(ggplot2)
    yob <- project[["yob"]]
    dob <- project[["dob"]]
    given <- project[["given"]]
    folder <- project[["folder"]]
    project_path <- project[["project_path"]]
    top <- project[["top"]]
    # ------- Get seed
    if (is.null(seed)) {
        seed <- digest::digest2int(glue::glue("{yob}{dob}"))
    }
    # ------- Get palette 
    if ("palette" %in% names(project)) {
        if ("Scale" %in% class(project$palette)) {
            palette <- project$palette
        }
        else {
            palette <- scale_fill_manual(values = project$palette)
        }
    }
    else {
        cols <- c("#89B9BD", "#323348", "#A25543", "#FFD3B8", "#149698", "#8A151B","#F04635")
        palette <- scale_fill_gradientn(colors = cols)
    }
    # ------- Get plot_data from project
    if ("plot_data" %in% names(project)) {
        plotdf <- project[["plot_data"]]
        type <- attributes(project[["plot_data"]])$type
    }
    else {
        plotdf <- project[["data"]]
        plotdf$x_final <- plotdf$x
        plotdf$y_final <- plotdf$y
        type <- 'raw'
    }
    # ------- Get step if missing
    if (!'step' %in% colnames(plotdf)) plotdf$step <- factor(plotdf$year)
    # ------- Set the limit for filtering (based on a input date, input age, or no filtering)
    set.seed(seed)
    if (is.null(age) & !is.null(date)) {
        age <- as.numeric(as.Date(date, "%d/%m/%Y") - dob)
        limit <- round(age/365.25)
        date <- as.Date(
            date, 
            format = "%d/%m/%Y"
        )
    }
    else if (!is.null(age) & is.null(date)) {
        limit <- age
        age <- floor(age * 365.25)
        date <- dob + age
    } 
    else if (is.null(age) & is.null(date)) {
        limit <- top
        age <- top
        date <- NA
    } 
    # ------- Stop if queried age is higher than max computed age
    if (limit > top) {
        msg_warning("Age is higher than the maximum computed age. Retry with lower age.")
        stop()
    }
    # ------- Import plot df 
    df_base <- plotdf
    df <- df_base
    # ------- Filter nodes <= year
    df <- df %>% dplyr::filter(as.numeric(step) <= limit) 
    # ------- Make sure that data ranges are rescaled to 0-1
    df[, c('x_final', 'y_final')] <- fitXY(df[, c('x_final', 'y_final')])
    # ------- Inverse ratio if landscape mode
    if (orientation == 'landscape') {
        ratio <- 1/ratio
    }
    # ------- Rescale y's to ratio
    df$y_final <- scales::rescale(df$y_final, c(0, ratio))
    # ------- Add fill aesthetics and colored_tiles
    set.seed(seed)
    K <- round(nrow(df) / {2 - (limit/top)})
    K_rest <- nrow(df) - K
    df <- df %>%
        dplyr::mutate(
            fill = sample(1:age, nrow(.), replace = TRUE)/{3*nrow(.)}, # This picks a random value (continuous) for blacks palette
            colored_tile = sample(c(sample(2:1000, K, replace = TRUE), rep(1, K_rest))), # This picks a random value (continuous) for color palette. The fuller the network is, the more colored it becomes ( K -> nrow(df) )
            colored_tile_alpha = sample(c(sample(2:1000, K, replace = TRUE), rep(1, K_rest))) # For the colored points, gives a transparency
        )
    # ------- Discretize if discrete fill palete
    if ('ScaleDiscrete' %in% class(palette)) {
        df <- df %>%
            dplyr::mutate(
                fill = 0, # No blacks palette
                colored_tile = factor(sample(1:length(project$palette), nrow(.), replace = TRUE)), # Pick colors from palette range
                colored_tile_alpha = 1 # For the colored points, gives a transparency
            )
    }
    # ------- Add background
    if (background) {
        baseplot <- backgroundTiles()
    } 
    else {
        baseplot <- ggplot(df)
    }
    p <- baseplot
    # ------- Add inner circles 
    col_scale <- scale_fill_gradient(low = '#E74900', high = '#E74900')
    p <- p + 
        ggforce::geom_voronoi_tile(
            data = df, aes(x = x_final, y = y_final, fill = fill, group = -1L), 
            alpha = 0.2,
            expand = unit(-2, 'mm'), 
            radius = unit(0.25, 'mm'), 
            max.radius = max.radius, 
            normalize = TRUE, 
            asp.ratio = 1/ratio
        ) + col_scale + ggnewscale::new_scale_fill()
    # ------- Add circles
    col_scale <- scale_fill_gradient(low = '#3E00E7', high = '#3E00E7')
    p <- p + 
        ggforce::geom_voronoi_tile(
            data = df, aes(x = x_final, y = y_final, fill = fill, group = -1L), 
            alpha = 0.2,
            expand = unit(-.1, 'mm'), 
            radius = unit(0.25, 'mm'), 
            max.radius = max.radius, 
            normalize = TRUE, 
            asp.ratio = 1/ratio
        ) + col_scale + ggnewscale::new_scale_fill()
    # ------- Add outer circles 
    col_scale <- scale_fill_gradient(low = '#007A0A', high = '#007A0A')
    p <- p + 
        ggforce::geom_voronoi_tile(
            data = df, aes(x = x_final, y = y_final, fill = fill, group = -1L), 
            alpha = 0.2,
            expand = unit(-.3, 'mm'), 
            radius = unit(0.25, 'mm'), 
            max.radius = max.radius, 
            normalize = TRUE, 
            asp.ratio = 1/ratio
        ) + col_scale 
    # ------- Add outer circles 
    p <- p + 
        ggforce::geom_voronoi_tile(
            data = df, aes(x = x_final, y = y_final, group = -1L), 
            fill = 'white', 
            alpha = 1,
            expand = unit(-3, 'mm'), 
            radius = unit(0.25, 'mm'), 
            max.radius = max.radius, 
            normalize = TRUE, 
            asp.ratio = 1/ratio
        ) 
    # ------- Add black dots 
    p <- p + 
        ggplot2::geom_point(
            data = df, aes(x = x_final, y = y_final), 
            fill = 'black',
            col = 'black',
            alpha = 1,
            size = 0.4
        )
    # ------- Add theme arguments
    p <- p + 
        theme_void() + 
        theme(legend.position = 'none') 
    if (!is.null(theme.args)) p <- p + theme.args
    # ------- Zoom & frame the plot
    p <- zoomify(p, zoom = zoom, ratio = ratio, seed = seed)
    # ------- Save plot
    if (is.null(file)) {
        if (is.null(path)) {
            plot_path <- glue::glue("{project_path}/plots/plot_{given}_{dob}_{type}_{date}.pdf")
        }
        else {
            plot_path <- glue::glue("{path}/plot_{given}_{dob}_{type}_{date}.pdf")
        }
    }
    else {
        plot_path <- file
    }
    if (!dir.exists(dirname(plot_path))) 
        dir.create(dirname(plot_path), showWarnings = FALSE)
    ggsave(plot = p, plot_path, width = maxsize/ratio, height = maxsize, dpi = dpi, units = 'in', limitsize = FALSE)
    msg_success(glue::glue("Plot saved in {plot_path}"))
    # ------- Return project
    project$plot_final <- df
    project$plot <- p
    invisible(project)
}

plotAges <- function(
    project, 
    ages, 
    ...
) {
    for (age in ages) {
        plotArt(
            project, 
            date = NULL, 
            age = age,  
            ...
        )
    }
}

backgroundTiles <- function(density = 1000, radius = 0.001, fill = '#949494', col = '#bdbdbdc9') {
    set.seed(1)
    df <- data.frame(
        x = sample(seq(-1, 2, length.out = 10000), density, replace = TRUE),
        y = sample(seq(-1, 2, length.out = 10000), density, replace = TRUE), 
        fill = sample(seq(0, 0.3, length.out = 10000), density, replace = TRUE)
    )
    p <- ggplot(df, aes(x, y, fill = fill)) + 
        ggforce::geom_delaunay_tile(
            radius = radius, 
            col = col
        ) + 
        ggplot2::scale_fill_brewer(palette = 'Greys') + 
        theme_void() +
        theme(legend.position = 'none') + 
        coord_cartesian(xlim = c(-0.05, 1.05), ylim = c(-0.05, 1.05), expand = TRUE, clip = "off") 
}

zoomify <- function(p, zoom = 1, ratio = 1, seed) {
    zooms <- c(0, 0.25, 0.33, 0.5, 0.6, 0.66, 0.71, 0.75, 0.77, 0.80)/2
    set.seed(seed)
    xlims <- c(
        zooms[zoom],
        1 - zooms[zoom]
    ) + sample(seq(0, 0.97, by = 0.01), 1) * ((1-zoom)/10)/2
    ylims <- range(c(
        (zooms[zoom]) * ratio,
        (1 - zooms[zoom]) * ratio
    ) + sample(seq(0, 0.97, by = 0.01), 1) * ((1-zoom)/(10))/2)
    q <- p + coord_cartesian(
        xlim = xlims, ylim = ylims, expand = TRUE, clip = "off"
    )
    return(q)
}
