library(shiny)
library(phyloseq)
library(qiime2R)
library(dplyr)
library(tibble)
library(here)
#remotes::install_github("twbattaglia/btools")
library(btools)
library(microbiome)
library(ggplot2)
library(reticulate)
library(RColorBrewer)

options(shiny.maxRequestSize = 30 * 1024^2)
setwd(here("06_shiny_app"))

hospital_donor <- read.csv(here("01_tidy_data", "hosp_donor.csv.gz"), header = TRUE, sep = ",")


# Define UI for slider demo app ----
ui <- fluidPage(
  # App title ----
  titlePanel("Compare alpha diversity"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(


    ######### Questionnaire ############

    # Sidebar to demonstrate various slider options ----
    sidebarPanel(

      # Input: Age ----
      sliderInput("age", "Age [years]:",
                  min = 1, max = 120,
                  value = 1),

      # Input: Height ----
      sliderInput("height", "Height [cm]:",
                  min = 100, max = 220,
                  value = 100),

      # Input: Weight ----
      sliderInput("weight", "Weight [kg]:",
                  min = 30, max = 150,
                  value = 30),

      radioButtons("sex", "Sex:",            #Is it important?
                   c("Male" = "male",
                     "Female" = "female")),

      radioButtons("cdiff", "Previous C. difficile history:",
                   c("Yes" = "yes",
                     "No" = "no")),

      radioButtons("antibio", "Previous antibiotic treatment:",
                   c("Yes" = "yes",
                     "No" = "no")),

      fileInput("upload_FASTQ", NULL, buttonLabel = "Upload FASTQ file directory", multiple = TRUE),

      tags$head(tags$style(type="text/css", "
             #loadmessage {
               position: fixed;
               top: 0px;
               left: 0px;
               width: 100%;
               padding: 5px 0px 5px 0px;
               text-align: center;
               font-weight: bold;
               font-size: 100%;
               color: #000000;
               background-color: #CCFF66;
               z-index: 105;
             }
          ")),

      conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                       tags$div("FASTQ files are being processed...",id="loadmessage")),

      actionButton("submit", "Submit"),
      actionButton("save", "Save the record")

    ),

    # Main panel for displaying outputs ----
    mainPanel(
      tabsetPanel(type = "tabs",
                  # Output: Table summarizing the values entered ----
                  tabPanel("Tables",
                           #tableOutput("phyloseq"),
                           tableOutput("metadata"),
                           br(),
                           br(),
                           textOutput("text_search")
                  ),

                  tabPanel("Richness", plotOutput("plot_richness"),
                           br(),
                           br(),
                           textOutput("text_faith")
                           #,downloadButton('downloadPlot3', 'Download Plot')
                  )
                  ,
                  tabPanel("Evenness", plotOutput("plot_evenness"),
                           br(),
                           br(),
                           textOutput("text_gini")
                  )
      )
    )
  )
)

server <- function(input, output, session) {

  observeEvent(input$submit, {
    req(input$upload_FASTQ)

    conda_list()[[1]][1] %>%
      use_condaenv(required = TRUE)

    tmp_dir <- tempdir()

    # Copy uploaded files to temporary directory
    file.copy(input$upload_FASTQ[[1, 'datapath']], file.path(tmp_dir, input$upload_FASTQ[[1, 'name']]))
    file.copy(input$upload_FASTQ[[2, 'datapath']], file.path(tmp_dir, input$upload_FASTQ[[2, 'name']]))

    uploaded_files <- c(input$upload_FASTQ[[1, 'name']], input$upload_FASTQ[[2, 'name']])
    all_files <- list.files(tmp_dir)
    extra_files <- setdiff(all_files, uploaded_files)
    unlink(file.path(tmp_dir, extra_files), recursive = TRUE, force = TRUE)

    system(paste("bash script.sh", tmp_dir))


    ## ASVs
    ASVs <- read_qza(here("06_shiny_app", "table-dada2.qza"))

    ## Rooted tree
    tree <- read_qza(here("06_shiny_app", "rooted-tree.qza"))

    ## Taxonomy
    taxonomy <- read_qza(here("06_shiny_app", "taxonomy.qza"))

    gini <- read.delim(here("06_shiny_app", "gini.tsv"))
    faith <- read.delim(here("06_shiny_app", "faith.tsv"))
    # chao1 <- read.delim(here("06_shiny_app", "chao1.tsv"))
    # fisher <- read.delim(here("06_shiny_app", "fisher.tsv"))
    # margalef <- read.delim(here("06_shiny_app", "margalef.tsv"))
    # menhinick <- read.delim(here("06_shiny_app", "menhinick.tsv"))
    # simpson <- read.delim(here("06_shiny_app", "simpson.tsv"))
    # strong <- read.delim(here("06_shiny_app", "strong.tsv"))
    # pielou <- read.delim(here("06_shiny_app", "pielou.tsv"))
    # shannon <- read.delim(here("06_shiny_app", "shannon.tsv"))

    # Reactive expression to create data frame of all input values ----
    metadata <- reactive({
      data.frame(
        Name = c("Sample_ID",
                 "Age",
                 "Height",
                 "Weight",
                 "BMI",
                 "Sex",
                 "Cdiff",
                 "Antibiotics",
                 "Gini",
                 "Faith_PD"),
        Value = as.character(c(colnames(ASVs$data)[1],
                               input$age,
                               input$height,
                               input$weight,
                               round(input$weight/((input$height/100)^2), digits = 1),
                               input$sex,
                               input$cdiff,
                               input$antibio,
                               gini$gini_index,
                               faith$faith_pd
                               # ,chao1$chao1,
                               # fisher$fisher,
                               # margalef$margalef,
                               # menhinick$menhinick,
                               # simpson$simpson,
                               # strong$strong,
                               # pielou$pielou_evenness,
                               # shannon$shannon_entropy

        )),

        stringsAsFactors = FALSE) %>% column_to_rownames(var = "Name") %>% t()

    })

    # physeq <- qza_to_phyloseq(here("06_shiny_app", "table-dada2.qza"), here("06_shiny_app", "rooted-tree.qza"), here("06_shiny_app", "taxonomy.qza"))
    #
    # alpha_metrics <- estimate_richness(physeq, measure = c("Shannon", "Simpson", "Chao1", "Fisher"))
    # alpha_metrics$Faith_PD <- estimate_pd(physeq)$PD
    # alpha_metrics$Pielou <- evenness(physeq, index = "pielou")$pielou
    # alpha_metrics$Gini <- unname(inequality(physeq))

    # output$phyloseq <- renderTable({
    #   alpha_metrics
    # })


    # metadata()$Gini <- gini$gini_index
    # metadata()$Faith_PD <- faith$faith_pd


    output$metadata <- renderTable({
      metadata()
    })

    degfree <- length(hospital_donor)-1

    # T-test for Gini index
    tval_gini <- (gini$gini_index - mean(hospital_donor$gini_index)) / (sd(hospital_donor$gini_index)*sqrt((length(hospital_donor$gini_index)+1) / length(hospital_donor$gini_index)))
    pval_gini <- pt(tval_gini, df=degfree)

    # T-test for Faith PD
    tval_faith <- (faith$faith_pd - mean(hospital_donor$faith_pd)) / (sd(hospital_donor$faith_pd)*sqrt((length(hospital_donor$faith_pd)+1) / length(hospital_donor$faith_pd)))
    pval_faith <- pt(tval_faith, df=degfree)


    plotInput1 <- reactive({

      violin <- hospital_donor %>% ggplot(aes(x = gini_index, y = 1)) +
        geom_violin(alpha=0.5, linewidth=1, color = "#1B9E77", fill="#8ae3c9")+
        geom_boxplot(width=0.1, color="#1B9E77", alpha=0.2, linewidth=1) +
        geom_vline(aes(xintercept = gini$gini_index), color = "#D95F02", linetype = "dashed", linewidth=1)+
        xlab("Gini index") +
        ylab("")+
        theme_classic() +
        theme(legend.position="none", axis.text.y=element_blank(), axis.ticks.y=element_blank())

      violin
    })

    plotInput2 <- reactive({

      violin <- hospital_donor %>% ggplot(aes(x = faith_pd, y = 1)) +
        geom_violin( alpha=0.5, color = "#4E84C4", fill="#9bc3f2", linewidth=1)+
        geom_boxplot(width=0.1, color="#4E84C4", alpha=0.2, linewidth=1) +
        geom_vline(aes(xintercept = faith$faith_pd), color = "#D95F02", linetype = "dashed", linewidth=1)+
        xlab("Faith Phylogenetic Distance") +
        ylab("")+
        theme_classic() +
        theme(legend.position="none", axis.text.y=element_blank(), axis.ticks.y=element_blank())
      violin
    })

    output$plot_richness <- renderPlot({
      print(plotInput1())
    })

    output$text_faith <- renderText({
      paste("The probability of this sample belonging to healthy donor population is:", pval_faith)
    })

    output$plot_evenness <- renderPlot({
      print(plotInput2())
    })

    output$text_gini <- renderText({
      paste("The probability of this sample belonging to healthy donor population is:", pval_gini)
    })


    observeEvent(input$save, {
      #Add record to the CSV file
      if (file.exists(here("06_shiny_app","data_record.csv"))){

        print("File exists!")

        write.table(metadata(), "data_record.csv",
                    append = TRUE,
                    sep = ",",
                    col.names = FALSE,
                    row.names = FALSE,
                    quote = FALSE)
      } else {

        file.create(file.path(here("06_shiny_app","data_record.csv")))

        write.csv(metadata(), "data_record.csv", row.names=FALSE, quote = FALSE)
      }

    })

    unlink(file.path(here("06_shiny_app"), c("demux-paired-end.qza", "table-dada2.qza", "rep-seqs-dada2.qza", "denoising-stats-dada2.qza",
                                             "aligned-rep-seqs.qza", "masked-aligned-rep-seqs.qza", "unrooted-tree.qza", "rooted-tree.qza", "taxonomy.qza",
                                             "faith_pd_vector.qza", "gini_index_vector.qza", "gini.tsv", "faith.tsv", "chao1_vector.qza",
                                             "fisher_alpha_vector.qza", "margalef_vector.qza", "menhinick_vector.qza", "simpson_vector.qza",
                                             "strong_vector.qza", "pielou_vector.qza", "shannon_vector.qza", "chao1.tsv", "fisher_alpha.tsv",
                                             "margalef.tsv", "menhinick.tsv", "simpson.tsv", "strong.tsv", "pielou.tsv", "shannon.tsv")))

  })
}

shinyApp(ui = ui, server = server)
