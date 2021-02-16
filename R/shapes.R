#' addRingShape
#'
#' @param project 
#' @param force 
#'
#' @return project
#'
#' @export

addRingShape <- function(project, force = FALSE) {
    `%>%` <- tidyr::`%>%`
    yob <- project[["yob"]]
    dob <- project[["dob"]]
    given <- project[["given"]]
    folder <- project[["folder"]]
    top <- project[["top"]]
    project_path <- project[["project_path"]]
    plotdf_path <- glue::glue("{project_path}/plotdf.rds")
    # ------- Calculating distance to center of circle
    plotdf <- project[["data"]]
    x0 <- plotdf$x[which.min(abs(plotdf$x - mean(plotdf$x)))]
    y0 <- plotdf$y[which.min(abs(plotdf$y - mean(plotdf$y)))]
    dists <- sapply(1:nrow(plotdf), function(K) {sqrt((plotdf$x[K] - x0)^2 + (plotdf$y[K] - y0)^2)})
    dists <- max(dists) - dists
    # ------- Bin radii
    dists_cuts <- cut(dists, 100, include.lowest = TRUE)
    # ------- Remove rows further than optimal distance
    max_dist <- which(levels(dists_cuts) == names(which.max(table(dists_cuts)))) %>% 
        levels(dists_cuts)[.] %>% 
        str_replace('\\(', '') %>% 
        str_replace(',.*', '') %>% 
        as.numeric()
    # ------- Add noise to XY
    plotdf[, c('x_final', 'y_final')] <- addNoise(plotdf[, c('x', 'y')], digest::digest2int("{given}"), scale = 200)
    # ------- Saving all
    plotdf <- plotdf[dists > max_dist,]
    plotdf$year <- as.numeric(cut(dists[dists > max_dist], 100, include.lowest = TRUE))
    plotdf$dist <- dists[dists > max_dist]
    msg_success(glue::glue("Added ring shape!"))
    project[["data_final"]] <- plotdf
    attr(project[["data_final"]], 'type') <- 'ring'
    return(project)
}

#' addLoessShape
#'
#' @param project 
#' @param force 
#'
#' @return project
#'
#' @export

addLoessShape <- function(project, force = FALSE) {
    `%>%` <- tidyr::`%>%`
    yob <- project[["yob"]]
    dob <- project[["dob"]]
    given <- project[["given"]]
    folder <- project[["folder"]]
    top <- project[["top"]]
    cores <- project[["cores"]]
    project_path <- project[["project_path"]]
    plotdf_path <- glue::glue("{project_path}/plotdf.rds")
    # ------- Calculating distance to LOESS curve
    msg_note(glue::glue("Mapping curve and calculating distance..."))
    plotdf <- project[["data"]]
    plotdf <- distanceToLoess(plotdf, cores = 4)
    dists <- plotdf$dist
    # ------- Cut distance to curve values by a slowly decreasing log vec. 
    y <- log(1:100)
    x <- seq(20, 100, length.out = length(y))
    m <- lm(y ~ log(x))
    # plot(x, predict(m),type='l',col='blue')
    y_scaled <- predict(m, x = seq(min(dists), max(dists), length.out = top)) # Redistribution of values
    y_scaled <- scales::rescale(y_scaled, range(dists)) # Rescaled redistribution of values
    dists_cuts <- cut(dists, y_scaled)
    # ------- Rescaling x's
    msg_note(glue::glue("Rescaling x values..."))
    plotdf$x_rescaled <- plotdf$x * 3
    # ------- Rescaling y's
    msg_note(glue::glue("Rescaling y values..."))
    rescale_fac <- sample(scales::rescale(dnorm(seq(0, 1, length.out = nrow(plotdf)), mean = 0.15, sd = 0.15), c(0, 1)))
    plotdf$y_rescaled <- plotdf$y - plotdf$dist * (1 - rescale_fac * (plotdf$year + 1)/100)
    # ------- Rotate XY
    plotdf[, c('x_rescaled', 'y_rescaled')] <- rotateXY(plotdf[, c('x_rescaled', 'y_rescaled')])
    # ------- Add noise to XY
    plotdf[, c('x_final', 'y_final')] <- addNoise(plotdf[, c('x_rescaled', 'y_rescaled')], digest::digest2int("{given}"), scale = 120)
    # ------- Saving all
    plotdf$dist <- dists
    msg_success(glue::glue("Added curve shape!"))
    project[["data_final"]] <- plotdf
    attr(project[["data_final"]], 'type') <- 'loess'
    return(project)
}

#' addCurveShape
#'
#' @param project 
#' @param force 
#'
#' @return project
#'
#' @export

addLineShape <- function(project, force = FALSE) {
    `%>%` <- tidyr::`%>%`
    yob <- project[["yob"]]
    dob <- project[["dob"]]
    given <- project[["given"]]
    folder <- project[["folder"]]
    top <- project[["top"]]
    cores <- project[["cores"]]
    project_path <- project[["project_path"]]
    plotdf_path <- glue::glue("{project_path}/plotdf.rds")
    # ------- Calculating distance to LOESS curve
    msg_note(glue::glue("Mapping curve and calculating distance..."))
    plotdf <- project[["data"]]
    plotdf <- distanceToStraightLine(plotdf, cores = 4)
    dists <- plotdf$dist
    # ------- Cut distance to curve values by a slowly decreasing log vec. 
    y <- log(1:100)
    x <- seq(20, 100, length.out = length(y))
    m <- lm(y ~ log(x))
    # plot(x, predict(m),type='l',col='blue')
    y_scaled <- predict(m, x = seq(min(dists), max(dists), length.out = top)) # Redistribution of values
    y_scaled <- scales::rescale(y_scaled, range(dists)) # Rescaled redistribution of values
    dists_cuts <- cut(dists, y_scaled)
    # ------- Rescaling x's
    msg_note(glue::glue("Rescaling x values..."))
    plotdf$x_rescaled <- plotdf$x * 3
    # ------- Rescaling y's
    msg_note(glue::glue("Rescaling y values..."))
    rescale_fac <- sample(scales::rescale(dnorm(seq(0, 1, length.out = nrow(plotdf)), mean = 0.15, sd = 0.15), c(0, 1)))
    plotdf$y_rescaled <- plotdf$y - plotdf$dist * (1 - rescale_fac * (plotdf$year + 1)/100)
    # ------- Rotate XY
    plotdf[, c('x_rescaled', 'y_rescaled')] <- rotateXY(plotdf[, c('x_rescaled', 'y_rescaled')])
    # ------- Add noise to XY
    plotdf[, c('x_final', 'y_final')] <- addNoise(plotdf[, c('x_rescaled', 'y_rescaled')], digest::digest2int("{given}"), scale = 80)
    # ------- Saving all
    plotdf$dist <- dists
    msg_success(glue::glue("Added line shape!"))
    project[["data_final"]] <- plotdf
    attr(project[["data_final"]], 'type') <- 'line'
    return(project)
}
