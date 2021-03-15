#' plotDots
#'
#' @export

plotDots <- plotArt

# The idea with plotLines is to plot outer dots and all lines between them

#' plotLinks
#'
#' @export


plotLinks <- function(
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
    keep = 0.1,
    n.edges = 10,
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
        type <- 'links'
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
        msg_warning(glue::glue("Queried age ({limit}) is higher than the maximum computed age ({top}). Retry with lower age."))
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
    
    # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< HERE START CUSTOMIZATION
    msg_note("Computing nodes to keep...")
    dat <- cbind(
        df, 
        df %>% 
            dplyr::select(x_final, y_final) %>% 
            dplyr::rename(x = x_final, y = y_final) %>% 
            getDistanceToCenter(keep = keep)
    ) %>% dplyr::filter(keep) 
    msg_note("Computing segments...")
    segms <- dat %>% 
        dplyr::select(x_final, y_final) %>% 
        dplyr::rename(x = x_final, y = y_final) %>% 
        turnDataIntoSegms(n = n.edges)
    #
    p <- p + 
        geom_segment(data = segms, aes(x, y, xend = xend, yend = yend), alpha = 0.2)
    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> HERE ENDS CUSTOMIZATION

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


