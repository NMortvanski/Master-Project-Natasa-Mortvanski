library(shiny)
library(plotly)
library(gridlayout)
library(bslib)
library(DT)


ui <- grid_page(
  layout = c(
    "header  header  plot2",
    "sidebar sidebar plot2",
    "sidebar sidebar area3",
    "sidebar sidebar area3"
  ),
  row_sizes = c(
    "110px",
    "1.97fr",
    "0.3fr",
    "0.73fr"
  ),
  col_sizes = c(
    "250px",
    "0.29fr",
    "1.71fr"
  ),
  gap_size = "1rem",
  grid_card(
    area = "sidebar",
    card_header("Input metadata"),
    card_body_fill(
      sliderInput(
        inputId = "age",
        label = "Age [years]:",
        min = 1,
        max = 120,
        value = 1,
        width = "100%"
      ),
      sliderInput(
        inputId = "height",
        label = "Height [cm]:",
        min = 100,
        max = 220,
        value = 100,
        width = "100%"
      ),
      sliderInput(
        inputId = "weight",
        label = "Weight [kg]:",
        min = 30,
        max = 150,
        value = 30,
        width = "100%"
      ),
      radioButtons(
        inputId = "sex",
        label = "Sex:",
        choices = list("Male" = "male", "Female" = "female"),
        width = "100%"
      ),
      radioButtons(
        inputId = "cdiff",
        label = "Previous C. difficile history:",
        choices = list("Yes" = "yes", "No" = "no"),
        width = "100%"
      ),
      radioButtons(
        inputId = "antibio",
        label = "Previous antibiotic treatment:",
        choices = list("Yes" = "yes", "No" = "no"),
        width = "100%"
      ),
      numericInput(
        inputId = "faith",
        label = "Insert value of Faith PD:",
        value = 0
      ),
      numericInput(
        inputId = "gini",
        label = "Insert value of Gini index:",
        value = 0
      ),
      actionButton(
        inputId = "submit",
        label = "Submit",
        width = "100%"
      ),
      actionButton(inputId = "save", label = "Save the record")
    )
  ),
  grid_card_text(
    area = "header",
    content = "Compare diversity",
    alignment = "start",
    is_title = FALSE
  ),
  grid_card(
    area = "plot2",
    card_body_fill(
      tabsetPanel(
        tabPanel(
          title = "Richness",
          plotOutput(outputId = "plot_richness")
        ),
        tabPanel(
          title = "Evenness",
          plotOutput(outputId = "plot_evenness")
        )
      )
    )
  ),
  grid_card(
    area = "area3",
    card_body_fill(
      "Table",
      DTOutput(outputId = "myTable", width = "100%")
    )
  )
)


server <- function(input, output) {
   
  output$distPlot <- renderPlotly({
    # generate bins based on input$bins from ui.R
    plot_ly(x = ~ faithful[, 2], type = "histogram")
  })
  
  output$bluePlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
  
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = "steelblue", border = "white")
  })
  
  output$myTable <- renderDT({
    head(faithful, input$numRows)
  })
}

shinyApp(ui, server)
  

