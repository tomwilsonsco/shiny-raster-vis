library(shiny)
library(shinyFiles)
require("data")

raster_file <- list.files("data", full.names = TRUE)[1]

raster_use_rgb <- brick(raster_file)

raster_use_gray <- raster(raster_file)


ui <- fluidPage(
  selectInput("plot_type", label = "Raster Plot Type", choices = c(RGB = "rgb", grayscale = "grayscale"), selected="grayscale"),
  conditionalPanel(
    condition = "input.plot_type == 'rgb'",
    selectInput("red_choice", label = "Red Band", choices = c(1, 2, 3, 4), selected = 3),
    selectInput("green_choice", label = "Green Band", choices = c(1, 2, 3, 4), selected = 2),
    selectInput("blue_choice", label = "Blue Band", choices = c(1, 2, 3, 4), selected = 1)
  ),
  conditionalPanel(
    condition = "input.plot_type == 'grayscale'",
    selectInput("gray_choice", label = "Grayscale Band", choices = c(1), selected = 1)
  ),
  plotOutput("ras_plot", inline=FALSE)
)


server <- function(input, output) {
  extent_to_use <- generate_random_window(raster_use_rgb)
  output$ras_plot <- renderPlot({
    if (input$plot_type == "rgb") {
      plot_rgb_raster(input_raster, extent_to_use, image_scale = 1500, red_channel = input$red_choice, green_channel = input$green_choice, blue_channel = input$blue_choice)
    }
    else {
      plot_grayscale_raster(raster_use_gray, extent_to_use, gray_channel=input$gray_choice)
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)

