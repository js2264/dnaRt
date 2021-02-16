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
    # ------- Cut radius values by a slowly decreasing log vec. 
    y <- log(1:100)
    x <- seq(20, 100, length.out = length(y))
    m <- lm(y ~ log(x))
    # plot(x, predict(m),type='l',col='blue')
    dists <- abs(dists - quantile(dists, 0.60)) # Rescale dists so that 0 is ~ 0.75 from the center 
    y_scaled <- predict(m, x = seq(min(dists), max(dists), length.out = top)) # Redistribution of values
    y_scaled <- (y_scaled - min(y_scaled)) / (max(y_scaled) - min(y_scaled)) * max(dists) # Rescaled redistribution of values
    dists_cuts <- cut(dists, y_scaled)
    # ------- Saving all
    plotdf$dist <- dists
    plotdf$dist_cut <- dists_cuts
    msg_success(glue::glue("Added ring shape!"))
    project[["data"]] <- plotdf
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

addCurveShape <- function(project, force = FALSE) {
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
    # ------- Cut distance to curve values by a slowly decreasing log vec. 
    y <- log(1:100)
    x <- seq(20, 100, length.out = length(y))
    m <- lm(y ~ log(x))
    # plot(x, predict(m),type='l',col='blue')
    y_scaled <- predict(m, x = seq(min(dists), max(dists), length.out = top)) # Redistribution of values
    y_scaled <- (y_scaled - min(y_scaled)) / (max(y_scaled) - min(y_scaled)) * max(dists) # Rescaled redistribution of values
    dists_cuts <- cut(dists, y_scaled)
    # ------- Rescaling y's
    msg_note(glue::glue("Rescaling y values..."))
    plotdf$y_rescaled <- plotdf$y - plotdf$dist + plotdf$dist * sample(seq(0.01, 0.1), 1) * as.numeric(dists_cuts)
    plotdf$y <- plotdf$y_rescaled
    # ------- Saving all
    plotdf$dist <- dists
    plotdf$dist_cut <- dists_cuts
    msg_success(glue::glue("Added curve shape!"))
    project[["data"]] <- plotdf
    return(project)
}

