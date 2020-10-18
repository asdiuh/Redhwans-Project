library(shiny)

ui <- fluidPage(
  shiny::titlePanel('testing'),
  shiny::tabPanel(title = 'tabsetPanel')
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)