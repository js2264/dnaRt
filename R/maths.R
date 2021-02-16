distanceToLoess <- function(data, cores = 4) {
    # -- Loess curve
    model <- loess(y ~ x, data = data)
    y <- predict(model, newdata = data.frame(x = data$x), se = FALSE)
    loe <- data.frame(x = data$x, y = y)
    data$loe_y <- loe$y
    data$dist <- data$y - data$loe_y
    return(data)
}