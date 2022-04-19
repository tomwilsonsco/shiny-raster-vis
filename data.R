library(raster)
library(RStoolbox)
library(ggspatial)
library(ggplot2)
library(sf)


random_xy <- function(input_raster, buffer_x = 100, buffer_y = 100) {
  x_min <- extent(input_raster)[1] + buffer_x
  x_max <- extent(input_raster)[2] - buffer_x
  y_min <- extent(input_raster)[3] + buffer_y
  y_max <- extent(input_raster)[4] - buffer_y
  x_rand <- sample(x_min:x_max, 1)
  y_rand <- sample(y_min:y_max, 1)
  c(x_rand, y_rand)
}

window_from_xy <- function(input_raster, xy_coords, pixels_x, pixels_y) {
  x_col <- colFromX(input_raster, xy_coords[1])
  y_row <- rowFromY(input_raster, xy_coords[2])
  min_x <- xFromCol(input_raster, x_col - round(pixels_x / 2))
  max_x <- xFromCol(input_raster, x_col + round(pixels_x / 2))
  min_y <- yFromRow(input_raster, y_row + round(pixels_y / 2))
  max_y <- yFromRow(input_raster, y_row - round(pixels_y / 2))
  extent(min_x, max_x, min_y, max_y)
}

plot_rgb_raster <- function(input_raster, plot_extent, image_scale = c(0, 1500), red_channel = 3, green_channel = 2, blue_channel = 1) {
  ggplot() +
    ggRGB(input_raster,
      r = red_channel, g = green_channel,
      b = blue_channel, ext = plot_extent,
      limits = image_scale, scale = image_scale[2], ggLayer = TRUE
    ) +
    coord_sf(
      datum = st_crs(input_raster),
      xlim = c(plot_extent[1], plot_extent[2]),
      ylim = c(plot_extent[3], plot_extent[4])
    ) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    annotation_scale(location = "bl", width_hint = 0.6, text_cex = 1.5, style = "bar", plot_unit = "m") +
    theme_bw() +
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      plot.title = element_text(size = 9),
      plot.margin = unit(c(0, 0, 0, 0), "pt")
    )
}

plot_grayscale_raster <- function(input_raster, plot_extent, image_scale = c(0, 1500), gray_channel = 1) {
  gray_raster <- raster(input_raster, layer = gray_channel)
  cropped_raster <- crop(input_raster, plot_extent)
  ggplot() +
    ggR(cropped_raster, layer = gray_channel, ggLayer = TRUE, geom_raster = TRUE) +
    scale_fill_gradient(low = "black", high = "white", limits = image_scale, na.value = "white") +
    coord_sf(
      datum = st_crs(cropped_raster),
      xlim = c(plot_extent[1] + 10, plot_extent[2] - 10),
      ylim = c(plot_extent[3] + 10, plot_extent[4] - 10)
    ) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    annotation_scale(location = "bl", width_hint = 0.6, text_cex = 1.5, style = "bar", plot_unit = "m") +
    theme_bw() +
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      plot.title = element_text(size = 9),
      plot.margin = unit(c(0, 0, 0, 0), "pt"),
      legend.position = "none"
    )
}
