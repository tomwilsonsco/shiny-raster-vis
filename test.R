library(shiny)
library(raster)
source("data.R")

raster_file <- list.files("data", full.names = TRUE)[1]

raster_use_rgb <- brick(raster_file)

generate_random_window <- function(input_raster, pixels_x = 1000, pixels_y = 500) {
  x_size <- ncol(input_raster) - pixels_x
  x_sample <- sample(x_size, size = 1)
  y_size <- nrow(input_raster) - pixels_y
  y_sample <- sample(y_size, size = 1)
  min_x <- xFromCol(input_raster, x_sample)
  min_y <- yFromRow(input_raster, y_sample)
  extent(min_x, min_x + pixels_x, min_y, min_y + pixels_y)}


ui <- fluidPage(
  plotOutput("ras_plot", inline=FALSE)
)


server <- function(input, output, session) {
  output$ras_plot <- renderPlot({
    extent_to_use <- generate_random_window(raster_use_rgb)
    plot(raster_use_rgb, ext=extent_to_use)
  })
}

# Run the application
shinyApp(ui = ui, server = server)

