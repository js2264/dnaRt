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
