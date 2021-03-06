distanceToRing <- function(data) {
    # -- Calculate distance to ring
    x0 <- data$x[which.min(abs(data$x - mean(data$x)))]
    y0 <- data$y[which.min(abs(data$y - mean(data$y)))]
    dists <- sapply(1:nrow(data), function(K) {sqrt((data$x[K] - x0)^2 + (data$y[K] - y0)^2)})
    dists <- max(dists) - dists
    data$dist_shape <- dists
    # ------- Find optimal max distance
    dists_cuts <- cut(dists, 100, include.lowest = TRUE)
    max_dist <- which(levels(dists_cuts) == names(which.max(table(dists_cuts)))) %>% 
        levels(dists_cuts)[.] %>% 
        stringr::str_replace('\\(', '') %>% 
        stringr::str_replace(',.*', '') %>% 
        as.numeric()
    # ------- Return results
    attr(data, 'shape') <- list('df' = data.frame(x = x0, y = y0))
    attr(data, 'opt_max_dist') <- max_dist
    return(data)
}

distanceToLoess <- function(data) {
    # -- Loess curve
    model <- loess(y ~ x, data = data, span = 0.5)
    y <- predict(model, newdata = data.frame(x = data$x), se = FALSE)
    loe <- data.frame(x = data$x, y = y)
    data$dist_shape <- distanceToPoints(data, loe)
    attr(data, 'shape') <- list('df' = loe)
    return(data)
}

distanceToStraightLine <- function(data) {
    # -- Distance to straight line
    model <- lm(y ~ x, data = data)
    y <- predict(model, newdata = data.frame(x = data$x), se = FALSE)
    li <- data.frame(x = data$x, y = y)
    data$dist_shape <- distanceToPoints(data, li)
    attr(data, 'shape') <- list('df' = li)
    return(data)
}

distanceToEllipsis <- function(data, seed) {
    # ----- Distance to elliptic curve
    el <- ellipsisCurve(seed = seed, a = 1, b = 1, c = 1)
    data$dist_shape <- distanceToPoints(data, el)
    attr(data, 'shape') <- el
    return(data)
}

ellipsisCurve <- function(nrow = 1000, seed = 1, a = NULL, b = NULL, c = NULL) {
    set.seed(seed)
    xlims <- c(sample(seq(-2, 0, length.out = 100), 1), sample(seq(0, 2, length.out = 100), 1))
    if (is.null(a)) a <- sample(seq(1, 2, length.out = 20))
    if (is.null(b)) b <- sample(seq(1, 2, length.out = 20))
    if (is.null(c)) c <- sample(seq(1, 2, length.out = 20))
    angle <- sample(seq(0, 2*pi, length.out = 100), 1)
    #
    x <- c(seq(xlims[1], xlims[2], length.out = nrow/4), seq(xlims[1], xlims[2], length.out = nrow/4))
    y <- suppressWarnings(c(sqrt(a*(x^3)+b*x+c), -sqrt(a*(x^3)+b*x+c)))
    d <- data.frame(x = x, y = y) %>% 
        fitXY(c(0.2, 0.8)) %>%
        rotateXY(angle = angle) %>% 
        fitXY(c(0.2, 0.8))
    return(d)
}

fitXY <- function(xy, lims = c(0, 1)) {
    res <- data.frame(
        scales::rescale(xy[, 1], lims), 
        scales::rescale(xy[, 2], lims) 
    )
    colnames(res) <- colnames(xy)
    return(res)
}

rotateXY <- function(xy, angle = pi/4) {
    M <- matrix( c(cos(angle), -sin(angle), sin(angle), cos(angle)), 2, 2 )
    res <- as.data.frame(as.matrix(xy) %*% M)
    colnames(res) <- colnames(xy)
    return(res)
}

addNoise <- function(xy, seed, grain = 200) {
    set.seed(seed)
    xy[, 1] <- xy[, 1] + sample(seq(-5,5,0.01), nrow(xy), replace = TRUE)*grain/100
    xy[, 2] <- xy[, 2] + sample(seq(-5,5,0.01), nrow(xy), replace = TRUE)*grain/100
    return(xy)
}

distanceToPoints <- function(data, points) {
    geoms <- points %>% 
        dplyr::filter(!is.na(x), !is.na(y)) %>% 
        dplyr::mutate(geom = sf::st_sfc(lapply(1:nrow(.), function(K) sf::st_point(as.numeric(.[K,]))))) %>% 
        dplyr::pull(geom)
    sf_points <- data %>% 
        dplyr::select(x, y) %>% 
        dplyr::filter(!is.na(x), !is.na(y)) %>% 
        dplyr::mutate(geom = sf::st_sfc(lapply(1:nrow(.), function(K) sf::st_point(as.numeric(.[K,]))))) %>% 
        dplyr::pull(geom)
    nearest_el <- geoms[sf::st_nearest_feature(sf_points, geoms)]
    d <- sapply(1:nrow(data), function(K) sf::st_distance(sf_points[K], nearest_el[K]))
    return(d)
}

getDistanceToCenter <- function(data, keep = 0.05) {
    # -- Calculate distance to ring
    x0 <- data$x[which.min(abs(data$x - mean(data$x)))]
    y0 <- data$y[which.min(abs(data$y - mean(data$y)))]
    dists <- sapply(1:nrow(data), function(K) {sqrt((data$x[K] - x0)^2 + (data$y[K] - y0)^2)})
    data$dist <- dists
    # ------- Bin by angle
    data$angle <- apply(data, 1, function(row) {
        an <- atan((row['x'] - x0) / (row['y'] - y0))
        if (row['x'] > x0 & row['y'] > y0) {
            an <- pi/2 - an
        }
        else if (row['x'] <= x0 & row['y'] > y0) {
            an <- pi/2 - an
        }
        else if (row['x'] <= x0 & row['y'] <= y0) {
            an <- 3*pi/2 - an
        }
        else if (row['x'] > x0 & row['y'] <= y0) {
            an <- 3*pi/2 - an
        }
        return(an)
    })
    data$binned_angle <- cut(data$angle, breaks = nrow(data)*keep)
    # ------- Find optimal max distance
    data$keep <- sapply(1:nrow(data), function(K) {
        row <- data[K,]
        sub <- dplyr::filter(data, binned_angle == row$binned_angle)
        sub <- sub[which.max(sub$dist),]
        return(row$x == sub$x & row$y == sub$y)
    })
    # ------- Return results
    return(data[, c('dist', 'angle', 'keep')])
}

