library(shiny)

ui <- fluidPage(
  dateInput("date", label = "Date of sampling:", value = NULL),
  textOutput("date_output")
)
server <- function(input, output, session) {

  output$date_output <- renderText({
   # print(as.character(input$date))
    print(input$date)

  })
}

shinyApp(ui = ui, server = server)
