#' msg_success
#'
#' @export

msg_success <- function(...) {
    x <- crayon::green(clisymbols::symbol$tick)
    timeof <- format(Sys.time(), format = "%R")
    message(glue::glue(
        "  ", 
        x,
        "  [{timeof}]: ", 
        paste(..., collapse = " ")
    ))
}

#' msg_warning
#'
#' @export

msg_warning <- function(...) {
    x <- crayon::red(clisymbols::symbol$cross)
    timeof <- format(Sys.time(), format = "%R")
    message(glue::glue(
        "  ", 
        x,
        "  [{timeof}]: ", 
        paste(..., collapse = " ")
    ))
}

#' msg_note
#'
#' @export

msg_note <- function(...) {
    x <- crayon::blue(clisymbols::symbol$circle_filled)
    timeof <- format(Sys.time(), format = "%R")
    message(glue::glue(
        "  ", 
        x,
        "  [{timeof}]: ", 
        paste(..., collapse = " ")
    ))
}

#' addMeta
#'
#' @export

addMeta <- function(project, x, label) {
    project[[label]] <- x
    return(project)
}

#' addPalette
#'
#' @export

addPalette <- function(project, x) {
    project$palette <- x
    return(project)
}

#' checkPalette
#'
#' @export

checkPalette <- function(cols) {
    p <- data.frame(x = 1, y = 1, z = 1:length(cols), col = factor(cols, levels = cols)) %>% 
        ggplot(aes(x = x, y = y, fill = z)) + 
        geom_tile() + 
        scale_fill_gradientn(colors = cols) + 
        facet_wrap(~col) + 
        theme_void() + 
        coord_fixed() + 
        labs(fill = 'Gradient')
    return(p)
}

readImg <- function(img) {
    isLink <- strsplit(img, '/')[[1]][1] %in% c('https:', 'http:')
    ext <- tools::file_ext(img)
    if (isLink) {
        msg_note(glue::glue("Fetching img from {img}"))
        z <- glue::glue(tempfile(), '.{ext}')
        download.file(img, z)
        img <- z
    }
    if (ext == 'png') {
        img <- png::readPNG(img)
    }
    else if (ext == 'jpeg' | ext == 'jpg') {
        img <- jpeg::readJPEG(img)
    } 
    else {
        stop(msg_warning("Please provide a png or jpeg image"))
    }
    return(img)
}

rgb2hex <- function(cols) {
    rgb(cols[[1]], cols[[2]], cols[[3]], maxColorValue = 1)
}

#' getPaletteFromImg
#'
#' @export

getPaletteFromImg <- function(img, ncols = 5) {
    img <- readImg(img)
    cols <- lapply(1:dim(img)[1], function(i) {
        lapply(1:dim(img)[2], function(j) {rgb2hex(img[i, j, ])}) %>% unlist()
    }) %>% 
        do.call(c, .) %>% 
        table() %>% 
        sort(decreasing = TRUE) %>% 
        head(30) %>% 
        names()
    print(checkPalette(cols))
    return(cols[1:ncols])
}

filterBy <- function(project, label = 'dist_shape') {
    data <- project$plot_data
    top <- project$top
    vec <- data[[label]]
    data$step <- as.numeric(cut(vec, project$top, include.lowest = TRUE))
    project$plot_data <- data
    return(project)
}

turnDataIntoSegms <- function(dat, n = NULL) {
    nodes <- dat
    edges <- data.frame(
        from = rep(1:nrow(dat), each = nrow(dat)), 
        to = rep(1:nrow(dat), nrow(dat))
    ) %>% dplyr::filter(from != to)
    d <- lapply(1:nrow(edges), function(K) {
        from <- edges[K, 'from']
        to <- edges[K, 'to']
        data.frame(
            x = nodes[from, 'x'], 
            xend = nodes[to, 'x'],
            y = nodes[from, 'y'],
            yend = nodes[to, 'y']
        )
    }) %>% do.call(rbind, .)
    if (!is.null(n)) {
        d <- d %>% 
            dplyr::mutate(dist = sqrt((xend - x)^2 + (yend - y)^2)) %>%
            dplyr::group_by(x, y) %>% 
            dplyr::arrange(dist) %>% 
            dplyr::slice_min(dist, n = n)
    }
    return(d)
}