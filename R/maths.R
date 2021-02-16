distanceToLoess <- function(data, cores = 4) {
    # -- Loess curve
    model <- loess(y ~ x, data = data, span = 0.5)
    y <- predict(model, newdata = data.frame(x = data$x), se = FALSE)
    data$loe_y <- y
    data$dist <- data$y - y
    return(data)
}

distanceToStraightLine <- function(data, cores = 4) {
    # -- Distance to straight line
    model <- lm(y ~ x, data = data)
    y <- predict(model, newdata = data.frame(x = data$x), se = FALSE)
    data$loe_y <- y
    data$dist <- data$y - y
    return(data)
}

rotateXY <- function(xy, angle = pi/4) {
    M <- matrix( c(cos(angle), -sin(angle), sin(angle), cos(angle)), 2, 2 )
    res <- as.data.frame(as.matrix(xy) %*% M)
    return(res)
}

addNoise <- function(xy, seed, scale = 200) {
    set.seed(seed)
    xy[, 1] <- xy[, 1] + sample(seq(-5,5,0.01), nrow(plotdf), replace = TRUE)/scale
    xy[, 2] <- xy[, 2] + sample(seq(-5,5,0.01), nrow(plotdf), replace = TRUE)/scale
    return(xy)
}