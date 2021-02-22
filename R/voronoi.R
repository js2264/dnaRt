voronoiTiles <- function(
    img, 
    density = 10000
) {
    set.seed(1)
    dims <- c(length(unique(img$x)), length(unique(img$y)))
    df <- data.frame(
        x =  sample(seq(0, dims[1], length.out = 100000), density, replace = TRUE), 
        y = sample(seq(0, dims[2], length.out = 100000), density, replace = TRUE)
    )
    # ---- Get closest point in img for each point in df 
    geoms <- img %>% 
        dplyr::select(x, y) %>% 
        dplyr::filter(!is.na(x), !is.na(y)) %>% 
        dplyr::mutate(geom = sf::st_sfc(lapply(1:nrow(.), function(K) sf::st_point(as.numeric(.[K,]))))) %>% 
        dplyr::pull(geom)
    points <- df %>% 
        dplyr::select(x, y) %>% 
        dplyr::filter(!is.na(x), !is.na(y)) %>% 
        dplyr::mutate(geom = sf::st_sfc(lapply(1:nrow(.), function(K) sf::st_point(as.numeric(.[K,]))))) %>% 
        dplyr::pull(geom)
    df$hex <- img$hex[sf::st_nearest_feature(points, geoms)]
    df$hex <- factor(df$hex, levels = unique(df$hex))
    p <- ggplot(df, aes(x, dims[2]-y, group = -1L)) + 
        ggforce::geom_voronoi_tile(aes(fill = hex)) +
        ggplot2::scale_color_manual(values = levels(df$hex)) + 
        ggplot2::scale_fill_manual(values = levels(df$hex)) + 
        theme_void() +
        theme(legend.position = 'none') + 
        coord_fixed(ratio = 1) 
    return(p)
}

binImg <- function(img, res = 300) {
    ybins <- cut(1:dim(img)[[1]], breaks = seq(1, dim(img)[[1]], length.out = res+1), include.lowest = TRUE)
    xbins <- cut(1:dim(img)[[2]], breaks = seq(1, dim(img)[[2]], length.out = res/(dim(img)[[1]]/dim(img)[[2]])+1), include.lowest = TRUE)
    mat <- matrix(0, nrow = length(levels(ybins)), ncol = length(levels(xbins)))
    for (y in 1:nrow(mat)) {
        for (x in 1:ncol(mat)) {
            coords <- list(which(ybins == levels(ybins)[y]), which(xbins == levels(xbins)[x]))
            # mat[y, x] <- mean(img[coords[[1]], coords[[2]], 1]) + mean(img[coords[[1]], coords[[2]], 2]) + mean(img[coords[[1]], coords[[2]], 3])
            mat[y, x] <- rgb2hex(c(mean(img[coords[[1]], coords[[2]], 1]), mean(img[coords[[1]], coords[[2]], 2]), mean(img[coords[[1]], coords[[2]], 3])))
        }
    }
    df <- as.data.frame(mat) %>%
        setNames(1:ncol(.)) %>% 
        tibble::rownames_to_column('y') %>%
        dplyr::mutate(y = 1:nrow(.)) %>% 
        tidyr::gather('x', 'hex', -y)
    return(df)
}