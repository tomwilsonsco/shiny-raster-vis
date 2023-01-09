library(shiny)

source("data.R")

raster_file <- list.files("data", full.names = TRUE)[1]

raster_use_rgb <- brick(raster_file)

x_dim <- ncol(raster_use_rgb)

y_dim <- nrow(raster_use_rgb)


ui <- fluidPage(
  fluidRow(column(width = 12, radioButtons("plot_type", label = "Raster Plot Type", choices = c(rgb = "rgb", grayscale = "grayscale"), selected = "rgb", inline = TRUE))),
  fluidRow(
    conditionalPanel(
      condition = "input.plot_type == 'rgb'",
      column(
        4,
        numericInput("red_choice", label = "Red Band", value = 3, min = 1, max = 4, step = 1)
      ),
      column(
        4,
        numericInput("green_choice", label = "Green Band", value = 2, min = 1, max = 4, step = 1)
      ),
      column(
        4,
        numericInput("blue_choice", label = "Blue Band", value = 1, min = 1, max = 4, step = 1)
      )
    ),
    conditionalPanel(
      condition = "input.plot_type == 'grayscale'",
      numericInput("gray_choice", label = "Grayscale Band", value = 1, min = 1, max = 4, step = 1)
    ),
    fluidRow(
      column(
        width = 6, style = "padding-left:30px;",
        sliderInput("pixel_range", "Pixel min max", min = 0, max = 3000, value = c(0, 1500), step = 50)
      ),
      column(width = 3, numericInput("pixels_x", "Window size X (pixels)", min = 100, max = x_dim, value = as.integer(x_dim/4))),
      column(width = 3, numericInput("pixels_y", "Window size y (pixels)", min = 100, max = y_dim, value = as.integer(y_dim/4)))
    ),
    fluidRow(column(
      width = 12, style = "padding-left:30px;",
      actionButton("new_ext", "New random extent")
    )),
    fluidRow(
      br(),
      plotOutput("ras_plot", inline = FALSE)
    )
  )
)


server <- function(input, output, session) {
  extent_to_use <- eventReactive(input$new_ext, generate_random_window(raster_use_rgb, input$pixels_x, input$pixels_y), ignoreNULL = FALSE)
  output$ras_plot <- renderPlot({
    if (input$plot_type == "rgb") {
      plot_rgb_raster(raster_use_rgb, extent_to_use(),
        image_scale = c(input$pixel_range[1], input$pixel_range[2]),
        red_channel = input$red_choice, green_channel = input$green_choice, blue_channel = input$blue_choice
      )
    } else {
      plot_grayscale_raster(raster_use_rgb, extent_to_use(), image_scale = input$pixel_range, gray_channel = input$gray_choice)
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
