## Things to fix:

# validate if name already exists in the record
#
# add sample -> check the date, if date of sampling is later, copy previous entry, if it is earlier, generate new
#
# restart app when loading search records
#
#
# add download report option
#
# make buttons more pretty


#Load libraries

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
library(callr)
library(shinyjs)
library(lubridate)
library(ggrepel)
library(htmltools)

#-------------------------------------------------------------------------------
# Increase size limit for uploading files
options(shiny.maxRequestSize = 200 * 1024^2)

# Set working diractory to 06_shiny_app
setwd(here("06_shiny_app"))

# Load table of alpha diversityes from Hospital Clínic's donor samples
hospital_donor <- read.csv(here("01_tidy_data", "hosp_donor.csv.gz"), header = TRUE, sep = ",")

#-------------------------------------------------------------------------------

### UI ###

ui <- navbarPage("App Name",


                 useShinyjs(),

                 #---------------------------------------------------------------------------
                 # TAB PANEL "WELLCOME"

                 tabPanel("Wellcome",
                          titlePanel("Wellcome to *Application name*"),
                          br(),
                          br(),
                          fluidRow(
                            column(8, offset = 3, style = "background-color:#DCDCDC;",
                                   br(),
                                   h4("Description of the application"),
                                   br(),
                                   p("Write description here ....")
                            )
                          )
                 ),

                 #---------------------------------------------------------------------------
                 # TAB PANEL "NEW RECORD"

                 tabPanel("New Record",
                          titlePanel("Upload new patient record"),
                          br(),
                          tabsetPanel(type = "tabs",

                                      tabPanel("Input",
                                               br(),
                                               fluidRow(
                                                 column(10, offset = 1, style = "background-color:#DCDCDC;",
                                                        radioButtons("option", h4("Choose type of input:"),
                                                                     c("Add new record" = "new_record", "Add sample to existing record" = "add_sample"))
                                                 ),

                                                 column(5, offset = 1,
                                                        br(),
                                                        h3("Input patient's information")
                                                 ),

                                                 column(12, offset = 4,
                                                        br(),
                                                        br(),

                                                        # Input: Patient's name ----
                                                        textInput("name", "Patient's name:")
                                                 )
                                               ),

                                               # Conditional panel for importing data of a new patient ----
                                               conditionalPanel(condition = "input.option == 'new_record' ",
                                                                fluidRow(
                                                                  column(5, offset = 1,

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
                                                                                     value = 30)
                                                                  ),

                                                                  column(5, offset = 1,
                                                                         br(),
                                                                         br(),
                                                                         br(),

                                                                         # Input: Sex ----
                                                                         radioButtons("sex", "Sex:",            #Is it important?
                                                                                      c("Male" = "male", "Female" = "female")),

                                                                         # Input: C. difficile infection ----
                                                                         radioButtons("cdiff", "Previous C. difficile history:",
                                                                                      c("No" = "no", "Yes" = "yes"))
                                                                  )
                                                                )
                                               ),

                                               # Conditional panel for adding sample of an existing patient ----
                                               conditionalPanel(condition = "input.option == 'add_sample' ",
                                                                fluidRow(
                                                                  column(12, offset = 4,
                                                                         br(),
                                                                         textOutput("warning"),
                                                                         tags$head(tags$style("#warning{color: #cc3300;
                                           font-size: 15px;
                                           font-style: bold;
                                           }")
                                                                         ),
                                                                         br()
                                                                  )
                                                                )
                                               ),

                                               fluidRow(
                                                 column(5, offset = 1,

                                                        # Input: Antibiotic usage ----
                                                        radioButtons("antibio", "Previous antibiotic treatment:",
                                                                     c("No" = "no", "Yes" = "yes")
                                                        ),

                                                        # Input: Type of antibiotic ----
                                                        conditionalPanel(condition = "input.antibio == 'yes' ",
                                                                         textInput("antibio_therapy", "Which antibiotic was used?")
                                                        ),

                                                        # Input: FMT theraby ----
                                                        radioButtons("fmt", "FMT therapy:",
                                                                     c("No" = "no", "Yes" = "yes")
                                                        ),

                                                        # Input: Date of FMT ----
                                                        conditionalPanel(condition = "input.fmt == 'yes' ",
                                                                         dateInput("date_fmt", label = "When was FMT performed?", value = NULL)
                                                        )

                                                 ),

                                                 column(5, offset = 1,

                                                        # Input: Date of sampling ----
                                                        dateInput("date", label = "Date of sampling:", value = NULL),

                                                        # Input: Fastq files ----
                                                        fileInput("upload_FASTQ", NULL, buttonLabel = "Upload FASTQ file directory", multiple = TRUE)
                                                 ),

                                                 column(6, offset=5,
                                                        actionButton("submit", "Submit"),
                                                        actionButton("save", "Save new record"),
                                                        br(),
                                                        br()
                                                 )
                                               )
                                      ),

                                      # tabPanel("Tables",
                                      #     fluidRow(
                                      #         column(8, offset = 1,
                                      #             tableOutput("metadata")
                                      #         )
                                      #     )
                                      # ),

                                      # Plot output for richness metric
                                      tabPanel("Richness",
                                               fluidRow(
                                                 column(7, offset = 1,
                                                        br(),
                                                        plotOutput("plot_richness")
                                                 ),

                                                 column(2, offset = 1,
                                                        br(),
                                                        br(),
                                                        textOutput("text_faith")
                                                 )
                                               )
                                      ),

                                      # Plot output for evenness metric
                                      tabPanel("Evenness",
                                               fluidRow(
                                                 column(7, offset = 1,
                                                        br(),
                                                        plotOutput("plot_evenness")
                                                 ),

                                                 column(2, offset = 1,
                                                        br(),
                                                        br(),
                                                        textOutput("text_gini")
                                                 )
                                               )
                                      )
                          )
                 ),

                 #---------------------------------------------------------------------------
                 # TAB PANEL "SEARCH PATIENT RECORDS"

                 # Search previously saved patient records
                 tabPanel("Search Patient Records",
                          h2("Search Through Patient Records"),
                          br(),
                          fluidRow(
                            column(8, offset = 3,
                                   textInput("patient_id", "Input patient's ID"),
                            )
                          ),
                          br(),

                          conditionalPanel(condition = "input.patient_id",
                                           fluidRow(
                                             column(8, offset = 2, style = "background-color:#DCDCDC;",
                                                    uiOutput("text_title")
                                             )
                                           ),
                                           fluidRow(
                                             column(8, offset = 2, style = "background-color:#DCDCDC;",
                                                    column(3,
                                                           uiOutput("text_1")
                                                    ),

                                                    column(6, offset = 3,
                                                           uiOutput("text_2")
                                                    )
                                             )
                                           ),
                                           br(),
                                           br(),
                                           fluidRow(
                                             column(11,
                                                    DT::DTOutput(outputId = "dt_table")
                                             )
                                           ),
                                           br(),

                                           fluidRow(
                                             column(10, offset = 1,
                                                    uiOutput("text_title_plot")
                                             )
                                           ),

                                           br(),
                                           fluidRow(
                                             column(5, offset = 1,
                                                    plotOutput('plot_records_gini', height = 300),
                                             ),

                                             column(5,
                                                    plotOutput('plot_records_faith', height = 300),
                                             )
                                           ),
                                           br(),
                                           br(),
                                           fluidRow(
                                             column(8, offset = 4,
                                                    downloadButton('downloadPlot3', 'Download Record Report'),
                                             )
                                           ),
                                           br(),
                                           br()
                          ),
                          shiny::includeScript("script.js")
                 )
)

#-------------------------------------------------------------------------------
##SERVER

server <- function(input, output, session) {

  # Load previously saved patient records
  record <- reactive({
    read.csv(here("06_shiny_app","data_record.csv"), header = TRUE, sep = ",")
  })

  # Check if patients' record file exists when "Add sample to existing record"
  # option is chosen. Show a warning if it doesnt exist and disable "submit"
  # and "save" button
  observeEvent(input$option, {

    if (input$option=="add_sample" & !file.exists(here("06_shiny_app","data_record.csv"))){
      shinyjs::disable("submit")
      shinyjs::disable("save")
      output$warning <- renderText({
        print("There is no patient record file. Try adding new records first.")
      })
    } else {
      shinyjs::enable("submit")
      shinyjs::enable("save")
    }
  })

  # CONTINUE THIS # # Validate Name input
  # patient_name <- reactive({
  #   name_exists <- input$name %in% record()$Name
  #   shinyFeedback::feedbackWarning("n", name_exists, "This name already exists in the database. You maybe want to add new sample for this patient?")
  #   input$name
  # })

  # When "Submit" button is activated forward input files to a Bash script
  # that calculates alpha diversity metrics, generate "metadata" dataframe
  # that stores all input information and forward it to Richness and Evenness
  # tab outputs.

  observeEvent(input$submit, {

    # Require file input to continue
    req(input$upload_FASTQ)

    #-----------------------------------------------------------------------
    # QIIME2 processing

    # Configurate the loading message
    showModal(modalDialog("FASTQ files are being processed...", footer=NULL))

    # Activate conda environment
    conda_list()[[1]][1] %>%
      use_condaenv(required = TRUE)

    # Create temporary directory to store new sample's Fastq files
    tmp_dir <- tempdir()

    # Copy uploaded files to temporary directory
    file.copy(input$upload_FASTQ[[1, 'datapath']], file.path(tmp_dir, input$upload_FASTQ[[1, 'name']]))
    file.copy(input$upload_FASTQ[[2, 'datapath']], file.path(tmp_dir, input$upload_FASTQ[[2, 'name']]))

    # Delete extra files and directories created by Shiny during the inport
    uploaded_files <- c(input$upload_FASTQ[[1, 'name']], input$upload_FASTQ[[2, 'name']])
    all_files <- list.files(tmp_dir)
    extra_files <- setdiff(all_files, uploaded_files)
    unlink(file.path(tmp_dir, extra_files), recursive = TRUE, force = TRUE)

    # Execute Bash script in R terminal
    system(paste("bash script.sh", tmp_dir))

    removeModal()

    #-----------------------------------------------------------------------
    # LOADING CREATED FILES

    ## ASVs
    ASVs <- read_qza(here("06_shiny_app", "table-dada2.qza"))

    ## Rooted tree
    tree <- read_qza(here("06_shiny_app", "rooted-tree.qza"))

    ## Taxonomy
    taxonomy <- read_qza(here("06_shiny_app", "taxonomy.qza"))

    # Alpha diversity files
    gini <- read.delim(here("06_shiny_app", "gini.tsv"))
    faith <- read.delim(here("06_shiny_app", "faith.tsv"))

    #-----------------------------------------------------------------------
    # CREATE METADATA DATAFRAME

    ### If we want to add new patient's record
    if(input$option=="new_record"){

      new_sample <- reactive({
        data.frame(
          Name = c("Sample_ID",
                   "Name",
                   "Date_of_sampling",
                   "Age",
                   "Height",
                   "Weight",
                   "BMI",
                   "Sex",
                   "Cdiff",
                   "Antibiotics",
                   "FMT",
                   "FMT_date",
                   "Gini",
                   "Faith_PD"),
          Value = c(colnames(ASVs$data)[1],
                    input$name,
                    as.character(input$date),
                    input$age,
                    input$height,
                    input$weight,
                    round(input$weight/((input$height/100)^2), digits = 1),
                    input$sex,
                    input$cdiff,
                    if(input$antibio == "no"){input$antibio} else {input$antibio_therapy},
                    input$fmt,
                    if(input$fmt == "no"){"None"} else {as.character(input$date_fmt)},
                    gini$gini_index,
                    faith$faith_pd

          ), stringsAsFactors = FALSE) %>% column_to_rownames(var = "Name") %>% t()
      })
      metadata <- new_sample()
    }

    ### If we want to add new sample of existing patient
    else if (input$option=="add_sample"){
      add_sample <- reactive({
        record() %>% filter(Name == input$name) %>%
          mutate(Sample_ID = colnames(ASVs$data)[1]) %>%
          mutate(Gini = gini$gini_index) %>%
          mutate(Faith_PD = faith$faith_pd) %>%
          mutate(Date_of_sampling = as.character(input$date)) %>%
          mutate(Antibiotics = if(input$antibio == "no"){input$antibio} else {input$antibio_therapy}) %>%
          mutate(FMT = input$fmt) %>%
          mutate(if(input$fmt == "no"){"None"} else {as.character(input$date_fmt)})
      })
      metadata <- add_sample()
    }

    #-----------------------------------------------------------------------
    # APPLY MODIFIED T-TEST

    degfree <- length(hospital_donor)-1

    # T-test for Gini index
    tval_gini <- (0.976 - mean(hospital_donor$gini_index)) / (sd(hospital_donor$gini_index)*sqrt((length(hospital_donor$gini_index)+1) / length(hospital_donor$gini_index)))
    #tval_gini <- (gini$gini_index - mean(hospital_donor$gini_index)) / (sd(hospital_donor$gini_index)*sqrt((length(hospital_donor$gini_index)+1) / length(hospital_donor$gini_index)))
    pval_gini <- pt(tval_gini, df=degfree, lower.tail=FALSE)

    # T-test for Faith PD
    #tval_faith <- (faith$faith_pd - mean(hospital_donor$faith_pd)) / (sd(hospital_donor$faith_pd)*sqrt((length(hospital_donor$faith_pd)+1) / length(hospital_donor$faith_pd)))
    tval_faith <- (25.5 - mean(hospital_donor$faith_pd)) / (sd(hospital_donor$faith_pd)*sqrt((length(hospital_donor$faith_pd)+1) / length(hospital_donor$faith_pd)))
    pval_faith <- pt(tval_faith, df=degfree)

    #-----------------------------------------------------------------------
    # CREATE PLOTS

    plotInput1 <- reactive({
      hospital_donor %>% ggplot(aes(x = faith_pd, y = 1)) +
        geom_violin( alpha=0.5, color = "#4E84C4", fill="#9bc3f2", linewidth=1)+
        geom_boxplot(width=0.1, color="#4E84C4", alpha=0.2, linewidth=1) +
        #geom_vline(aes(xintercept = faith$faith_pd), color = "#D95F02", linetype = "dashed", linewidth=1)+
        geom_vline(aes(xintercept = 25.5), color = "#D95F02", linetype = "dashed", linewidth=1)+
        xlab("Faith Phylogenetic Distance") +
        ylab("")+
        theme_classic() +
        theme(legend.position="none", axis.text.y=element_blank(), axis.ticks.y=element_blank())
    })

    plotInput2 <- reactive({
      hospital_donor %>% ggplot(aes(x = gini_index, y = 1)) +
        geom_violin(alpha=0.5, linewidth=1, color = "#1B9E77", fill="#8ae3c9")+
        geom_boxplot(width=0.1, color="#1B9E77", alpha=0.2, linewidth=1) +
        #geom_vline(aes(xintercept = gini$gini_index), color = "#D95F02", linetype = "dashed", linewidth=1)+
        geom_vline(aes(xintercept = 0.976), color = "#D95F02", linetype = "dashed", linewidth=1)+
        xlab("Gini index") +
        ylab("")+
        theme_classic() +
        theme(legend.position="none", axis.text.y=element_blank(), axis.ticks.y=element_blank())
    })

    #-----------------------------------------------------------------------
    # OUTPUTS FOR "New record" PAGE

    # output$metadata <- renderTable({
    #     metadata
    # })

    # Output richness plot
    output$plot_richness <- renderPlot({
      print(plotInput1())
    })

    # Output text on Richness page
    output$text_faith <- renderText({
      paste("The probability of this sample belonging to healthy donor population is:", round(pval_faith, 3))
    })

    # Output evenness plot
    output$plot_evenness <- renderPlot({
      print(plotInput2())
    })

    # Output text on Evenness page
    output$text_gini <- renderText({
      paste("The probability of this sample belonging to healthy donor population is:", round(pval_gini, 3))
    })

    # When "Save" button is activated the created "metadata" dataframe is
    # saved as a new line in "data_records.csv" file.

    observeEvent(input$save, {
      # Append record to the existing CSV file
      if (file.exists(here("06_shiny_app","data_record.csv"))){
        write.table(metadata, "data_record.csv",
                    append = TRUE,
                    sep = ",",
                    col.names = FALSE,
                    row.names = FALSE,
                    quote = FALSE)
      }
      # Create new CSV file and add the first row
      else {
        file.create(file.path(here("06_shiny_app","data_record.csv")))
        write.csv(metadata, "data_record.csv", row.names=FALSE, quote = FALSE)
      }
    })

    # Delete all files created during the analysis
    unlink(file.path(here("06_shiny_app"), c("demux-paired-end.qza", "table-dada2.qza",
                                             "rep-seqs-dada2.qza", "denoising-stats-dada2.qza", "aligned-rep-seqs.qza",
                                             "masked-aligned-rep-seqs.qza", "unrooted-tree.qza", "rooted-tree.qza", "taxonomy.qza",
                                             "faith_pd_vector.qza", "gini_index_vector.qza", "gini.tsv", "faith.tsv")))

  })

  #---------------------------------------------------------------------------
  # OUTPUTS FOR "Search patient Records" PAGE


  # Output data table with patient records
  output$dt_table <- DT::renderDataTable(
    record() %>% filter(Name == input$patient_id )%>%
      select(Sample_ID, Date_of_sampling, FMT_date, Gini, Faith_PD),
    options = list(dom = 't'),
    server = FALSE
  )

  # Output richness progression plot
  output$plot_records_gini <- renderPlot({

    s = input$dt_table_rows_selected

    plot_data <- record() %>%
      filter(Name == input$patient_id ) %>%
      filter(row_number() %in% s)

    plot <- plot_data %>%
      mutate(Date_of_sampling = ymd(Date_of_sampling)) %>%
      ggplot(aes(x=Date_of_sampling, y= Gini, group = 1, label = plot_data$Date_of_sampling)) +
      geom_line(color = '#dfc27d')+
      geom_point(size=4) +
      geom_label_repel()+
      theme_classic() +
      theme(legend.position="none", axis.title.y=element_text(size=20), title =element_text(size=20), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
      ylab("Gini index")+
      xlab("Date of sampling")

    filtered_data <- plot_data %>%
      select(FMT_date)

    if (nrow(filtered_data) > 0) {
      date_of_FMT <- pull(filtered_data, FMT_date)[1]
      plot <- plot + geom_vline(xintercept = ymd(date_of_FMT), color = "red", linetype = "dashed") +
        geom_text(aes(x=ymd(date_of_FMT), label=paste("\nFMT: ", date_of_FMT), y= (max(plot_data$Gini) + min(plot_data$Gini))/2, colour="red", angle=90, size=20))
    }

    print(plot)
  })

  # Output evenness progression plot
  output$plot_records_faith <- renderPlot({

    s = input$dt_table_rows_selected

    plot_data <- record() %>%
      filter(Name == input$patient_id ) %>%
      filter(row_number() %in% s)


    plot <-  plot_data %>%
      mutate(Date_of_sampling = ymd(Date_of_sampling)) %>%
      ggplot(aes(x= Date_of_sampling, y= Faith_PD, group = 1, label = plot_data$Date_of_sampling)) +
      geom_line(color = '#dfc27d')+
      geom_point(size=4) +
      geom_label_repel()+
      theme_classic() +
      theme(legend.position="none", axis.title.y=element_text(size=20), title =element_text(size=20), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
      ylab("Gini index")+
      xlab("Date of sampling")


    filtered_data <-  plot_data %>%
      select(FMT_date)

    if (nrow(filtered_data) > 0) {
      date_of_FMT <- pull(filtered_data, FMT_date)[1]
      plot <- plot + geom_vline(xintercept = ymd(date_of_FMT), color = "red", linetype = "dashed")+
        geom_text(aes(x=ymd(date_of_FMT), label=paste("\nFMT: ", date_of_FMT), y= (max(plot_data$Faith_PD) + min(plot_data$Faith_PD))/2, colour="red", angle=90, size=15))
    }

    print(plot)
  })

  observeEvent(input$dt_table_rows_selected, {

    selected_data <- record() %>%
      filter(Name == input$patient_id ) %>%
      filter(row_number() %in% input$dt_table_rows_selected)

    output$text_title <- renderText({
      HTML(paste0("<h4>","Patient Information: ","</h4>"))
    })

    output$text_1 <- renderText({
      HTML(paste0("<p><span style='font-weight: bold;'> Age: </span> ", pull(selected_data, Age)[1], "</p>",
                  "<p><span style='font-weight: bold;'> Sex: </span> ", pull(selected_data, Sex)[1], "</p>",
                  "<p><span style='font-weight: bold;'> Weight: </span> ", pull(selected_data, Weight)[1], "</p>"))
    })

    output$text_2 <- renderText({
      HTML(paste0("<p><span style='font-weight: bold;'> Height:  </span>",  pull(selected_data, Height)[1], "</p>",
                  "<p><span style='font-weight: bold;'> C. difficile infection:  </span>",  pull(selected_data, Cdiff)[1], "</p>",
                  "<p><span style='font-weight: bold;'> Antibiotic therapy:  </span>",  pull(selected_data, Antibiotics)[1], "</p>"))
    })

    output$text_title_plot <- renderText({
      HTML(paste0("<h3>","Plots of patient's alpha diversity progress throught time: ","</h3>"))
    })

  })
}

shinyApp(ui = ui, server = server)
