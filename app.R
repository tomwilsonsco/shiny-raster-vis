library(shiny)
library(raster)
library(RStoolbox)
library(shinyFiles)


# Define UI for application that draws a histogram
ui <- fluidPage(
  selectInput("plot_type", label = "Raster Plot Type", choices = c(RGB = "rgb", grayscale = "grayscale")),
  conditionalPanel(
    condition = "input.plot_type == 'rgb'",
    selectInput("red_choice", label = "Red Band", choices = c(1, 2, 3, 4), selected=3),
    selectInput("green_choice", label = "Green Band", choices = c(1, 2, 3, 4), selected=2),
    selectInput("blue_choice", label = "Blue Band", choices = c(1, 2, 3, 4), selected=1)
  ),
  conditionalPanel(
      condition = "input.plot_type == 'grayscale'",
      selectInput("gray_choice", label = "Grayscale Band", choices = c(1), selected=1)
 ),
 textOutput("selected_plot")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$selected_plot <- renderText({
  if (input$plot_type == "rgb"){
    input$red_choice
  }
  else {
    input$gray_choice
  }
      
  })  
}

# Run the application
shinyApp(ui = ui, server = server)
