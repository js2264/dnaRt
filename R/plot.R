
#' plotArt
#'
#' @param project 
#' @param date 
#' @param age 
#' @param palette 
#' @param pdf 
#' @param theme.args 
#'
#' @return project (invisible)
#'
#' @export

plotArt <- function(project, date = NULL, age = NULL, palette = scale_fill_gradientn(colors = c("#89B9BD", "#323348", "#A25543", "#FFD3B8", "#149698", "#8A151B","#F04635")), pdf = NULL, theme.args = NULL) {
    `%>%` <- tidyr::`%>%`
    library(ggplot2)
    yob <- project[["yob"]]
    dob <- project[["dob"]]
    given <- project[["given"]]
    folder <- project[["folder"]]
    project_path <- project[["project_path"]]
    top <- project[["top"]]
    if ("data_final" %in% names(project)) {
        plotdf <- project[["data_final"]]
        type <- attributes(project[["data_final"]])$type
    }
    else {
        plotdf <- project[["data"]]
        plotdf$x_final <- plotdf$x
        plotdf$y_final <- plotdf$y
        type <- 'raw'
    }
    #
    set.seed(glue::glue(digest::digest2int("{yob}{dob}")))
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
    if (limit > top) {
        msg_warning("Age is higher than the maximum computed age. Retry with lower age.")
        stop()
    }
    ncols <- 1000
    # ------- Filter nodes <= year
    df <- plotdf %>% dplyr::filter(year <= limit) 
    # ------- Add aesthetics
    set.seed(digest::digest2int(given))
    K <- round(nrow(df) / {2 - (limit/top)})
    K_rest <- nrow(df) - K
    df <- df %>%
        dplyr::mutate(
            fill = sample(1:age, nrow(.), replace = TRUE)/{3*nrow(.)}, 
            standout = sample(c(sample(2:ncols, K, replace = TRUE), rep(1, K_rest))),
            standout_alpha = sapply(standout, function(x) ifelse(x == 1, 0, 0.6))
        )
    # ------- Plot graph 
    blacks <- scale_fill_gradient(low = 'white', high = '#000000')
    p <- ggplot(df) + 
        ggforce::geom_voronoi_tile(
            aes(x = x_final, y = y_final, fill = fill), alpha = 0.6,
            expand = unit(-.5, 'mm'), radius = unit(0.25, 'mm'), max.radius = 0.01
        ) + blacks + ggnewscale::new_scale_fill() +
        ggforce::geom_voronoi_tile(
            aes(x = x_final, y = y_final, fill = standout, alpha = standout_alpha), 
            expand = unit(-.25, 'mm'), radius = unit(0.25, 'mm'), max.radius = 0.0125
        ) + palette + 
        theme_void() + 
        coord_fixed() + 
        theme(legend.position = 'none') + 
        lims(
            x = c(min(plotdf$x_final)-0.05, max(plotdf$x_final)+0.05), 
            y = c(min(plotdf$y_final)-0.05, max(plotdf$y_final)+0.05)
        )
    if (!is.null(theme.args)) p <- p + theme.args
    # ---------- Save plot
    if (!dir.exists(glue::glue("{project_path}/plots"))) 
        dir.create(glue::glue("{project_path}/plots"))
    if (is.null(pdf)) {
        plot_path <- glue::glue("{project_path}/plots/plot_{type}_{date}.pdf")
    }
    else {
        plot_path <- pdf
    }
    ggsave(plot = p, plot_path, width = 30, height = 30)
    msg_success(glue::glue("Plot saved in {plot_path}"))
    # Return project
    invisible(project)
}
