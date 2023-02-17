library(plotly)
library(tidyverse)
library(shiny)
library(tibble)
library(flextable)
library(stats)
library(dplyr)

nbins <- 50

all_healthy <- read.csv("~/Desktop/master_project/project_analysis/01_tidy_data/AGP_healthy.csv")

# Identify numeric columns
# num_cols <- unlist(lapply(all_healthy[12:196], is.numeric)) 
num_cols <- unlist(lapply(all_healthy[10:194], is.numeric)) 


# Exclude numeric variables from all_healthy
all_healthy <- all_healthy[, !names(all_healthy) %in% names(num_cols)[num_cols == "TRUE"]]

ui <- fluidPage(titlePanel("Dynamic Variable Selection"),
                
                sidebarLayout(sidebarPanel(
                  selectInput(
                    inputId = "y1",
                    label = "Select alpha metric",
                    choices = names(all_healthy)[2:9]),
                  
                  selectInput(
                    inputId = "y2",
                    label = "Select first variable",
                    choices = names(all_healthy)[10:189]),
                  
                  checkboxInput("check_sec_var", "Check to activate second variable", FALSE),
                  
                  selectInput(
                    inputId = "y3",
                    label = "Select second variable",
                    choices = names(all_healthy)[10:189])),
                  
                  mainPanel(
                    tabsetPanel(type = "tabs",
                      tabPanel("Density Plots", plotOutput("ggplot_density"),
                               br(),
                               br(),
                               downloadButton('downloadPlot1', 'Download Plot')),
                      tabPanel("Box Plots", plotOutput("ggplot_box"),
                               br(),
                               br(),
                               downloadButton('downloadPlot2', 'Download Plot')),
                      tabPanel("Violin Plots", plotOutput("ggplot_violin"),
                               br(),
                               br(),
                               downloadButton('downloadPlot3', 'Download Plot')),
                      tabPanel("Wilcox Test", uiOutput("flex"),
                              br(),
                              br()
                              #,uiOutput("wilcoxCondition")
                              # ,checkboxGroupInput("wilcox", "Choose category of variable 2:",
                              #                       choiceValues = NULL
                              #                         )
                                 )
                      )
                    )
                # mainPanel(plotOutput(outputId = "ggplot_density"),
                #           plotOutput(outputId = "ggplot_box"),
                #           #tableOutput(outputId = "table")
                #           uiOutput(outputId = "flex")
                #           )
                ))

server <- function(session, input, output) {
  # dynamically pull variable in ggplot
  
  plotInput1 <- reactive({
    all_healthy_a <- all_healthy %>% filter(.data[[input$y2]] != "Unspecified" & .data[[input$y2]] != "")

    dens <- all_healthy_a %>%
      #ggplot(aes(x = .data[["shannon_entropy"]], color = .data[[input$y2]])) +
      ggplot(aes(x = .data[[input$y1]], color = .data[[input$y2]])) +
      geom_density(alpha=.2, fill="#CC79A7") +
      xlab(label = input$y1) + 
      ylab(label = "density") 
      #facet_wrap(vars(.data[[input$y3]]), ncol=2)
     #facet_wrap(vars(.data[[input$y3]]), nrow = all_healthy_a %>% dplyr::group_by(.data[[input$y3]]) %>% dplyr::summarise() %>% nrow)
    if(input$check_sec_var==FALSE){
      mean_line <- all_healthy_a %>% dplyr::group_by(.data[[input$y2]]) %>% dplyr::summarise(grp_mean=mean(.data[[input$y1]]))
      
      dens + geom_vline(data = mean_line, aes(xintercept = grp_mean, color = .data[[input$y2]]), linetype = "dashed")

    }else{
      mean_line <- all_healthy_a %>% dplyr::group_by(.data[[input$y2]], .data[[input$y3]]) %>% dplyr::summarise(grp_mean = mean(.data[[input$y1]]))
      
      dens <- dens + 
        geom_vline(data = mean_line, aes(xintercept = grp_mean, color = .data[[input$y2]]), linetype = "dashed")+ 
        facet_wrap(vars(.data[[input$y3]]), ncol=2)
    }
  })
    
  output$ggplot_density <- renderPlot({
    print(plotInput1())
  })
  
  plotInput2 <- reactive({
    all_healthy_a <- all_healthy %>% filter(.data[[input$y2]] != "Unspecified" & .data[[input$y2]] != "")
    
    box <- all_healthy_a %>%
      ggplot(aes(x = .data[[input$y1]], color = .data[[input$y2]])) +
      geom_boxplot() +
      labs(x = input$y1) 
      #facet_wrap(vars(.data[[input$y3]]), ncol=2)
     #facet_wrap(vars(.data[[input$y3]]), nrow = all_healthy_a %>% dplyr::group_by(.data[[input$y3]]) %>% dplyr::summarise() %>% nrow)
    
    if(input$check_sec_var==FALSE){
      box
    }else{
      box <- box + facet_wrap(vars(.data[[input$y3]]), ncol=2)
    }
  })
  
  output$ggplot_box <- renderPlot({
    print(plotInput2())
  })
  
  plotInput3 <- reactive({
    all_healthy_a <- all_healthy %>% filter(.data[[input$y2]] != "Unspecified" & .data[[input$y2]] != "") %>%
      dplyr::group_by(.data[[input$y2]]) %>% dplyr::mutate(m = mean(.data[[input$y1]])) 
    
    violin <- all_healthy_a %>%
      #ggplot(aes(x = .data[["shannon_entropy"]], color = .data[[input$y2]])) +
      ggplot(aes(x = .data[[input$y1]], y = reorder(.data[[input$y2]], -m), color = .data[[input$y2]])) +
      geom_violin()+
      geom_boxplot(width=0.1, color="grey", alpha=0.2) +
      scale_x_continuous(trans = 'log10') +
      labs(x = metric[i])+
      ylab("")+
      theme(legend.position="none") 
      
    if(input$check_sec_var==FALSE){
      mean_line <- all_healthy_a %>% dplyr::group_by(.data[[input$y2]]) %>% dplyr::summarise(grp_mean=mean(.data[[input$y1]]))
      
      violin + geom_vline(data = mean_line, aes(xintercept = grp_mean, color = .data[[input$y2]]), linetype = "dashed")
      
    }else{
      mean_line <- all_healthy_a %>% dplyr::group_by(.data[[input$y2]], .data[[input$y3]]) %>% dplyr::summarise(grp_mean = mean(.data[[input$y1]]))
      
      violin <- violin + 
        geom_vline(data = mean_line, aes(xintercept = grp_mean, color = .data[[input$y2]]), linetype = "dashed")+ 
        facet_wrap(vars(.data[[input$y3]]), ncol=2)
    }
    
  })
  
  output$ggplot_violin <- renderPlot({
    print(plotInput3())
  })
  
############?????????????###############
  
  ####1####
#   observeEvent(input$check_sec_var, {
#     #if (input$check_sec_var){
#     choices <- reactive({
#       all_healthy %>%
#         select(input$y3) %>%
#         unique()%>%
#         as.list()
#     })
#     
#     updateCheckboxGroupInput(session, 'wilcox', choices = choices())
#     #}
# }, ignoreInit = TRUE)
#   
  ####2####
   # output$wilcoxCondition <- renderUI({
   #    if(input$check_sec_var){
   #    selectInput(inputId = "wilcox",
   #                label = "Choose category of variable 2:",
   #                #choices = unique(all_healthy[[input$y3]]))
   #                choices = choices()
   #    )
   #                }
   #  })

  ####3####
  
  
  output$flex <- renderUI({
     if(input$check_sec_var){
       all_healthy_a <- all_healthy %>% filter(.data[[input$y3]] == input$wilcox)
     }else{
       all_healthy_a <- all_healthy %>% filter(.data[[input$y2]] != "Unspecified" & .data[[input$y2]] != "")
     }

    test <- pairwise.wilcox.test(all_healthy_a[[input$y1]], all_healthy_a[[input$y2]], p.adjust.method="none") %>%
      broom::tidy() %>% arrange(p.value)
    test <- add_column(test, p.adjusted = p.adjust(test$p.value, "fdr"), .after='p.value')

    return(flextable(test) %>% bold(~ p.value < 0.05, 3) %>% bold(~ p.adjusted < 0.05, 4) %>%
             htmltools_value())
  })
  
  
######################??????????????????####################
  
  output$downloadPlot1 <- downloadHandler(
    filename = function() { paste(input$y1, '_', input$y2, '_dens_plot.png', sep='') },
    content = function(file) {
      device <- function(..., width, height) grDevices::png(..., width = width, height = height, res = 300, units = "in")
      ggsave(file, plot = plotInput1(), device = device)
    }
  )
  
  output$downloadPlot2 <- downloadHandler(
    filename = function() { paste(input$y1, '_', input$y2, '_box_plot.png', sep='') },
    content = function(file) {
      device <- function(..., width, height) grDevices::png(..., width = width, height = height, res = 300, units = "in")
      ggsave(file, plot = plotInput2(), device = device)
    }
  )
  
}

shinyApp(ui = ui, server = server)
