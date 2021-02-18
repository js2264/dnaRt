#' addRingShape
#'
#' @param project 
#'
#' @return project
#'
#' @export

addRingShape <- function(project) {
    `%>%` <- tidyr::`%>%`
    # ------- Calculating distance to center of circle
    msg_note(glue::glue("Calculating distance to ring center..."))
    plotdf <- project[["data"]]
    plotdf <- distanceToRing(plotdf)
    # ------- Add noise to XYs
    plotdf[, c('x_final', 'y_final')] <- addNoise(plotdf[, c('x', 'y')], digest::digest2int(project[["given"]]), grain = 0.5)
    # --!!--- Filter out points further away than optimal max distance (to get a disc shape)
    plotdf <- plotdf[plotdf$dist_shape >= attr(plotdf, 'opt_max_dist'),]
    # ------- Saving all
    msg_success(glue::glue("Added ring shape!"))
    project[["plot_data"]] <- plotdf
    attr(project[["plot_data"]], 'type') <- 'ring'
    return(project)
}

#' addLoessShape
#'
#' @param project 
#'
#' @return project
#'
#' @export

addLoessShape <- function(project) {
    `%>%` <- tidyr::`%>%`
    # ------- Calculating distance to LOESS curve
    msg_note(glue::glue("Mapping curve and calculating distance..."))
    plotdf <- project[["data"]]
    plotdf <- distanceToLoess(plotdf)
    # --!!--- Rescaling x's
    msg_note(glue::glue("Rescaling x values..."))
    plotdf$x_rescaled <- plotdf$x * 3
    # --!!--- Rescaling y's
    msg_note(glue::glue("Rescaling y values..."))
    rescale_fac <- sample(scales::rescale(dnorm(seq(0, 1, length.out = nrow(plotdf)), mean = 0.15, sd = 0.15), c(0, 1)))
    plotdf$y_rescaled <- plotdf$y - plotdf$dist_shape * (1 - rescale_fac * (plotdf$year + 1)/100)
    # --!!--- Rotate XY
    plotdf[, c('x_rescaled', 'y_rescaled')] <- rotateXY(plotdf[, c('x_rescaled', 'y_rescaled')])
    # ------- Add noise to XY
    plotdf[, c('x_final', 'y_final')] <- addNoise(plotdf[, c('x_rescaled', 'y_rescaled')], digest::digest2int(project[["given"]]), grain = 1)
    # ------- Saving all
    msg_success(glue::glue("Added curve shape!"))
    project[["plot_data"]] <- plotdf
    attr(project[["plot_data"]], 'type') <- 'loess'
    return(project)
}

#' addLineShape
#'
#' @param project 
#'
#' @return project
#'
#' @export

addLineShape <- function(project) {
    `%>%` <- tidyr::`%>%`
    # ------- Calculating distance to LOESS curve
    msg_note(glue::glue("Mapping curve and calculating distance..."))
    plotdf <- project[["data"]]
    plotdf <- distanceToStraightLine(plotdf)
    # ------- Rescaling x's
    msg_note(glue::glue("Rescaling x values..."))
    plotdf$x_rescaled <- plotdf$x * 3
    # ------- Rescaling y's
    msg_note(glue::glue("Rescaling y values..."))
    rescale_fac <- sample(scales::rescale(dnorm(seq(0, 1, length.out = nrow(plotdf)), mean = 0.15, sd = 0.15), c(0, 1)))
    plotdf$y_rescaled <- plotdf$y - plotdf$dist_shape * (1 - rescale_fac * (plotdf$year + 1)/100)
    # ------- Rotate XY
    plotdf[, c('x_rescaled', 'y_rescaled')] <- rotateXY(plotdf[, c('x_rescaled', 'y_rescaled')])
    # ------- Add noise to XY
    plotdf[, c('x_final', 'y_final')] <- addNoise(plotdf[, c('x_rescaled', 'y_rescaled')], digest::digest2int(project[["given"]]), grain = 1.2)
    # ------- Saving all
    msg_success(glue::glue("Added line shape!"))
    project[["plot_data"]] <- plotdf
    attr(project[["plot_data"]], 'type') <- 'line'
    return(project)
}

#' addEllipticShape
#'
#' @param project 
#'
#' @return project
#'
#' @export

addEllipticShape <- function(project) {
    `%>%` <- tidyr::`%>%`
    # ------- Calculating distance to LOESS curve
    msg_note(glue::glue("Mapping ellipsis and calculating distance..."))
    plotdf <- project[["data"]]
    plotdf <- distanceToEllipsis(plotdf, digest::digest2int(project[["given"]]))
    # ------- Add noise to XY
    plotdf[, c('x_final', 'y_final')] <- addNoise(plotdf[, c('x', 'y')], digest::digest2int(project[["given"]]), grain = 0.2)
    # --!!--- Filter out points to make it prettier - like a thinner line
    plotdf <- plotdf[sample(1:nrow(plotdf), nrow(plotdf)/5),]
    # ------- Saving all
    msg_success(glue::glue("Added ellipsis shape!"))
    project[["plot_data"]] <- plotdf
    attr(project[["plot_data"]], 'type') <- 'ellipsis'
    attr(project[["plot_data"]], 'shape') <- attr(plotdf, 'shape')
    return(project)
}
