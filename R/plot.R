#' plotArt
#'
#' @param project 
#' @param date 
#' @param age 
#' @param pdf 
#' @param path 
#' @param zoom 
#' @param theme.args 
#'
#' @return project (invisible)
#'
#' @export

plotArt <- function(
    project, 
    date = NULL, 
    age = NULL,  
    pdf = NULL, 
    path = NULL,
    theme.args = NULL,
    zoom = 1
) {
    `%>%` <- tidyr::`%>%`
    library(ggplot2)
    yob <- project[["yob"]]
    dob <- project[["dob"]]
    given <- project[["given"]]
    folder <- project[["folder"]]
    project_path <- project[["project_path"]]
    top <- project[["top"]]
    # ------- Get palette 
    if ("palette" %in% names(project)) {
        palette <- project$palette
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
    if (!'step' %in% colnames(plotdf)) 
        plotdf$step <- factor(plotdf$year)
    # ------- Set the limit for filtering (based on a input date, input age, or no filtering)
    set.seed(digest::digest2int(glue::glue("{yob}{dob}")))
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
    # ------- Filter nodes <= year
    df <- plotdf %>% dplyr::filter(as.numeric(step) <= limit) 
    # ------- Make sure that data ranges are rescaled to 0-1
    df$x_final <- scales::rescale(df$x_final, c(0, 1))
    df$y_final <- scales::rescale(df$y_final, c(0, 1))
    # ------- Add fill aesthetics and colored_tiles
    set.seed(digest::digest2int(given))
    K <- round(nrow(df) / {2 - (limit/top)})
    K_rest <- nrow(df) - K
    df <- df %>%
        dplyr::mutate(
            fill = sample(1:age, nrow(.), replace = TRUE)/{3*nrow(.)}, 
            colored_tile = sample(c(sample(2:1000, K, replace = TRUE), rep(1, K_rest))),
            colored_tile_alpha = sapply(colored_tile, function(x) ifelse(x == 1, 0, 0.6))
        )
    # ------- Plot graph 
    blacks <- scale_fill_gradient(low = 'white', high = '#000000')
    p <- ggplot(df) + 
        ggforce::geom_voronoi_tile(
            aes(x = x_final, y = y_final, fill = fill), alpha = 0.6,
            expand = unit(-.5, 'mm'), radius = unit(0.25, 'mm'), max.radius = 0.01
        ) + blacks + ggnewscale::new_scale_fill() +
        ggforce::geom_voronoi_tile(
            aes(x = x_final, y = y_final, fill = colored_tile, alpha = colored_tile), 
            expand = unit(-.25, 'mm'), radius = unit(0.25, 'mm'), max.radius = 0.0125
        ) + palette + 
        theme_void() + 
        coord_cartesian(expand = FALSE) + 
        theme(legend.position = 'none') + 
        lims(
            x = c(min(df$x_final)-0.05, max(df$x_final)+0.05), 
            y = c(min(df$y_final)-0.05, max(df$y_final)+0.05)
        )
    if (!is.null(theme.args)) p <- p + theme.args
    # ------- Zoom plot
    if (zoom > 1) p <- zoomify(p, zoom)
    # ---------- Save plot
    if (is.null(pdf)) {
        if (is.null(path)) {
            plot_path <- glue::glue("{project_path}/plots/plot_{given}_{dob}_{type}_{date}.pdf")
        }
        else {
            plot_path <- glue::glue("{path}/plot_{given}_{dob}_{type}_{date}.pdf")
        }
    }
    else {
        plot_path <- pdf
    }
    basedir <- 
    if (!dir.exists(dirname(plot_path))) 
        dir.create(dirname(plot_path), showWarnings = FALSE)
    ggsave(plot = p, plot_path, width = 49, height = 49, units = 'in')
    msg_success(glue::glue("Plot saved in {plot_path}"))
    # Return project
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

addBackground <- function() {

}

zoomify <- function(p, factor = 2) {
    pp <- ggplot_build(p)
    xrange <- pp$layout$panel_params[[1]]$x.range
    yrange <- pp$layout$panel_params[[1]]$y.range
    xlims <- c(
        xrange[1]*(1 - factor/10),
        xrange[2]*(1 - factor/10)
    )
    ylims <- c(
        yrange[1]*(1 - factor/10),
        yrange[2]*(1 - factor/10)
    )
    p <- p + coord_cartesian(xlim = xlims, ylim = ylims, expand = FALSE)
    return(p)
}