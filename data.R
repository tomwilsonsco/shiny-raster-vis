library(raster)
library(RStoolbox)
library(ggspatial)
library(ggplot2)
library(sf)

raster_file <- list.files("data", full.names = TRUE)[1]

raster_use_rgb <- brick(raster_file)

raster_use_gray <- raster(raster_file)


generate_random_window <- function(input_raster, pixels_x = 1000, pixels_y = 500) {
  x_size <- ncol(input_raster) - pixels_x
  x_sample <- sample(x_size, size = 1)
  y_size <- nrow(input_raster) - pixels_y
  y_sample <- sample(y_size, size = 1)
  min_x <- xFromCol(input_raster, x_sample)
  min_y <- yFromRow(input_raster, y_sample)
  extent(min_x, min_x + pixels_x, min_y, min_y + pixels_y)
}

plot_rgb_raster <- function(input_raster, plot_extent, image_scale = 1500, red_channel = 3, green_channel = 2, blue_channel = 1) {
  ggplot() +
    ggRGB(input_raster,
      r = red_channel, g = green_channel,
      b = blue_channel, ext = plot_extent,
      limits = c(0, image_scale), scale = image_scale, ggLayer = TRUE
    ) +
    coord_sf(
      datum = st_crs(input_raster),
      xlim = c(plot_extent[1], plot_extent[2] - 10),
      ylim = c(plot_extent[3], plot_extent[4] - 10)
    ) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    annotation_scale(location = "bl", width_hint = 0.6, style = "bar", plot_unit = "m") +
    theme_bw() +
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      plot.title = element_text(size = 9),
      plot.margin = unit(c(0, 0, 0, 0), "pt")
    )
}

plot_grayscale_raster <- function(input_raster, plot_extent, image_scale = 1500, gray_channel = 1) {
  cropped_raster <- crop(input_raster, plot_extent)
  ggplot() +
    ggR(cropped_raster, layer = gray_channel, ggLayer = TRUE, geom_raster = TRUE) +
    scale_fill_gradient(low="black", high="white", limits=c(0, image_scale), na.value = "white") +
    coord_sf(
      datum = st_crs(cropped_raster),
      xlim = c(plot_extent[1], plot_extent[2] - 10),
      ylim = c(plot_extent[3], plot_extent[4] - 10)
    ) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    annotation_scale(location = "bl", width_hint = 0.6, style = "bar", plot_unit = "m") +
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