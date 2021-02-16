#' getRing
#'
#' @param project 
#' @param force 
#'
#' @return project
#'
#' @export

getRingRadius <- function(project, force = FALSE) {
    `%>%` <- tidyr::`%>%`
    yob <- project[["yob"]]
    dob <- project[["dob"]]
    given <- project[["given"]]
    folder <- project[["folder"]]
    top <- project[["top"]]
    project_path <- project[["project_path"]]
    plotdf_path <- glue::glue("{project_path}/plotdf.rds")
    # ------- Calculating distance to center of circle
    msg_note(glue::glue("Calculating radius..."))
    plotdf <- readRDS(project[["plotdf_path"]])
    x0 <- plotdf$x[which.min(abs(plotdf$x - mean(plotdf$x)))]
    y0 <- plotdf$y[which.min(abs(plotdf$y - mean(plotdf$y)))]
    dists <- sapply(1:nrow(plotdf), function(K) {sqrt((plotdf$x[K] - x0)^2 + (plotdf$y[K] - y0)^2)})
    # ------- Cut radius values by a modeled distribution 
    y <- log(1:100)
    x <- seq(20, 100, length.out = length(y))
    m <- lm(y ~ log(x))
    plot(x, predict(m),type='l',col='blue')
    # ------- Cut radius values by a slowly decreasing log vec. 
    dists <- abs(dists - quantile(dists, 0.60)) # Rescale dists so that 0 is ~ 0.75 from the center 
    y_scaled <- predict(m, x = seq(min(dists), max(dists), length.out = top)) # Redistribution of values
    y_scaled <- (y_scaled - min(y_scaled)) / (max(y_scaled) - min(y_scaled)) * max(dists) # Rescaled redistribution of values
    dists_cuts <- cut(dists, y_scaled)
    # ------- Saving all
    plotdf$dist <- dists
    plotdf$dist_cut <- dists_cuts
    msg_note(glue::glue("Saving plotting data in {plotdf_path}..."))
    saveRDS(plotdf, plotdf_path)
    msg_success(glue::glue("Radius computed!"))
    return(project)
}

