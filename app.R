library(shiny)
source("data.R")

raster_file <- list.files("data", full.names = TRUE)[1]

raster_use_rgb <- brick(raster_file)


ui <- fluidPage(
  selectInput("plot_type", label = "Raster Plot Type", choices = c(rgb = "rgb", grayscale = "grayscale"), selected = "rgb"),
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
    fluidRow(column(
      width = 4,
      style = "padding-left:30px",
      actionButton("new_ext", "New extent")
    )),
    fluidRow(
      br(),
      plotOutput("ras_plot", inline = FALSE)
    )
  )
)


server <- function(input, output, session) {
  extent_to_use <- eventReactive(input$new_ext, generate_random_window(raster_use_rgb), ignoreNULL = FALSE)
  # extent_to_use <- generate_random_window(raster_use_rgb)
  output$ras_plot <- renderPlot({
    if (input$plot_type == "rgb") {
      plot_rgb_raster(raster_use_rgb, extent_to_use(), image_scale = 1500, red_channel = input$red_choice, green_channel = input$green_choice, blue_channel = input$blue_choice)
    } else {
      plot_grayscale_raster(raster_use_rgb, extent_to_use(), gray_channel = input$gray_choice)
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
