# https://stackoverflow.com/questions/35717353/split-violin-plot-with-ggplot2
GeomSplitViolin <- ggproto("GeomSplitViolin", GeomViolin,
                           draw_group = function(self, data, ..., flipped_aes = FALSE) {
                             
                             grp <- data[1, "group"]
                             
                             newdata <- data %>%
                               flip_data(flipped_aes) %>%
                               # Find the points for the line to go all the way around
                               mutate(
                                 xminv = x - violinwidth * (x - xmin),
                                 xmaxv = x + violinwidth * (xmax - x),
                                 x = if (grp %% 2 == 1) xminv else xmaxv
                               ) %>%
                               arrange(if (grp %% 2 == 1) y else -y)
                             
                             newdata <- rbind(
                               newdata[1, ],
                               newdata,
                               newdata[nrow(newdata), ],
                               newdata[1, ]
                             )
                             newdata[
                               c(1, nrow(newdata) - 1, nrow(newdata)), "x"
                             ] <- round(newdata[1, "x"])
                             newdata$x <- newdata$x + ifelse(grp %% 2 == 1, -0.015, 0.015)
                             
                             newdata <- flip_data(newdata, flipped_aes)
                             
                             ggplot2:::ggname(
                               "geom_split_violin", GeomPolygon$draw_panel(newdata, ...)
                             )
                           }
)

geom_split_violin <- function(mapping = NULL, data = NULL, stat = "ydensity",
                              position = "identity", ...,
                              trim = TRUE, scale = "area", na.rm = FALSE,
                              show.legend = NA, inherit.aes = TRUE) {
  layer(
    geom = GeomSplitViolin, data = data, mapping = mapping, stat = stat,
    position = position, show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(trim = trim, scale = scale, na.rm = na.rm, ...)
  )
}
