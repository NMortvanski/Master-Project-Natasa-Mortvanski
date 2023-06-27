library(shiny)
library(dplyr)

# Sample data frame
df <- read.csv(here("06_shiny_app","data_record.csv"), header = TRUE, sep = ",")

ui <- fluidPage(
  titlePanel("Search Bar Example"),
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        selectizeInput(
          inputId = "searchme",
          label = "Search Bar",
          multiple = FALSE,
          choices = c("Search Bar" = "", paste0(LETTERS,sample(LETTERS, 26))),
          options = list(
            create = FALSE,
            placeholder = "Search Me",
            maxItems = '1',
            onDropdownOpen = I("function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}"),
            onType = I('function(str) {
                        var choices = [];
                        var searchText = str.toLowerCase();
                        var data = df$Sample_ID;
                        for (var i = 0; i < data.length; i++) {
                          var value = data[i].toLowerCase();
                          if (value.indexOf(searchText) !== -1) {
                            choices.push(data[i]);
                          }
                        }
                        this.clearOptions();
                        this.addOption({ value: "", text: "" });
                        this.addOptionGroup("Matches", choices);
                      }')
          )
        )
      )
    ),

    mainPanel(
      verbatimTextOutput("result"),
      tableOutput("data_row")
    )
  )
)

server <- function(input, output, session) {

  observe({
    # Display selected value
    output$result <- renderPrint({
      input$searchme
    })

    output$data_row <- renderTable({
      filter(df$Sample_ID == input$searchme)
    })
  })

}

shinyApp(ui, server)
