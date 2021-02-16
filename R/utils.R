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

#' getPaletteFromImg
#'
#' @export

getPaletteFromImg <- function(img, ncols = 5) {
    img <- jpeg::readJPEG(img)
    rgb_2_hex <- function(cols) {rgb(cols[[1]], cols[[2]], cols[[3]], maxColorValue = 1)}
    cols <- lapply(1:dim(img)[1], function(i) {
        lapply(1:dim(img)[2], function(j) {rgb_2_hex(img[i, j, ])}) %>% unlist()
    }) %>% 
        do.call(c, .) %>% 
        table() %>% 
        sort(decreasing = TRUE) %>% 
        head(30) %>% 
        names()
    print(checkPalette(cols))
    return(cols[1:ncols])
}


