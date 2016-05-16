#------------------------------------------------------------------#
# R-Projekt: Gotta Read Em All (GREA)
# Authors: Stanislaus Stadlmann
# Module: Statistical Programming with R (University of Göttingen)
#------------------------------------------------------------------#

#.............................
# PRELIMINARIES
#.............................

# Packages
library(shiny)
library(miniUI)

# Delete everything 
rm(list = ls())

# Set WD
path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(path)
rm(path)

# Source function to read data
source("function.R")

#.............................
# Add-In
#.............................

# Set max File size
options(shiny.maxRequestSize = 500*1024^2)

ui <- miniPage(
  # Title Bar
  gadgetTitleBar(title = "Gotta Read 'Em All"),
  
  # Tabstrips
  miniTabstripPanel(
    
    # Input Tab Panel
    miniTabPanel(
      "Input", icon = icon("folder-open"),
      miniContentPanel(
        fillRow(
          column(width = 8,
            # File Input
            fileInput("file", label = "Select file to read into R", width = "80%"),
            textInput("name_dataset", label = "Give your dataset a name", width = "90%")
          ),
          column(width = 8,
            strong(p("Further options (per filetype)", options = "align = top")),
            # Output for textlike files
            uiOutput("sep"), uiOutput("dec"), uiOutput("header"),
            # Output for xls, sav
            uiOutput("todf_button"), uiOutput("sheet_field")
          )
        ))),
    
    # Preview Tab Panel
    miniTabPanel(
      "Preview", icon = icon("table"),
      miniContentPanel(
        DT::dataTableOutput("preview")
        #textOutput("tableprint")
      ))
  )
)


server <- shinyServer(function(input, output) {
  
  # ------ REACTIVE FILE LOCATION MOVEMENT ------ #
  
  fileloc <- reactive({
    if (!is.null(input$file)) {
      # Store file
      inFile <- input$file
      
      # Rename the filename to be the same as before
      command1 <- paste0("mv ", inFile$datapath,
                         " ", 
                         dirname(inFile$datapath), "/",inFile$name)
      system(command1)
      
      # Get new filelocation
      paste0(dirname(inFile$datapath), "/", inFile$name)
    }
  })
  
  # ------ VALIDATION ------ #
  
  # Creating the dataset
  dataset <- reactive({
    
    # First try generating the dataset, if something goes wrong display error message
    validate(
      need(
        try(data <- GREA_fun(fileloc(), 
                             # Options for raw, csv, txt, asc, dat
                             header = input$option_header,
                             sep = input$option_sep, dec = input$option_dec,
                             into.dataframe = input$option_todf,
                             sheetIndex = input$option_sheetindex)
        ), "        1. Wrong options for generating df, or\n
        2. Outcome is not a df (for Excel and SPSS reader functions)")
    )
    data
  })
  
  # ------ PREVIEW TABLE ------ #
  
  # Render preview table
  output$preview <- DT::renderDataTable({
    dataset()
  }, options = list(autoWidth = FALSE, paging = TRUE, searching = FALSE,
                    info = FALSE, ordering = FALSE, processing = FALSE, scrollX = TRUE),
  class = "cell-border stripe")

  # ------ INTERACTIVE UI ------ #
  
  # Render the UI button with sep selections, if dataset is selected
  output$sep <- renderUI({
    # Get filetype
    filetype <- obtain_filetype(fileloc())
    
    # Render UI if a certain filetype is selected
    if (any(filetype == c("raw", "csv", "txt", "asc", "dat")))
      selectInput(inputId = "option_sep",
                  label = "Select Separator",
                  choices = c( "White Space" = " ", "Semicolon" = ";", "Tabstopp" = "\t", 
                              "Dot" = ".", "Comma" = ","))
  })

  # Render the UI button with dec selections, if dataset is selected
  output$dec <- renderUI({
    # Get filetype
    filetype <- obtain_filetype(fileloc())
    
    # Render UI if a certain filetype is selected
    if (any(filetype == c("raw", "csv", "txt", "asc", "dat")))
      selectInput(inputId = "option_dec",
                  label = "Select Decimal Mark",
                  choices = c("Dot" = ".", "Comma" = ","))
  })
  
  # Render the UI button with the option to get a header
  output$header <- renderUI({
    # Get filetype
    filetype <- obtain_filetype(fileloc())
    
    # Render UI if a certain filetype is selected
    if (any(filetype == c("raw", "csv", "txt", "asc", "dat")))
      checkboxInput(inputId = "option_header", label = "Display header")
  })
  
  # Render the "to dataframe" button
  output$todf_button <- renderUI({
    # Get filetype
    filetype <- obtain_filetype(fileloc())
    
    # Render UI if a certain filetype is selected
    if (any(filetype == "sav"))
      checkboxInput(inputId = "option_todf", label = "Into Dataframe?", 
                    value = TRUE)
  })
  
  # Render the "sheetIndex" field
  output$sheet_field <- renderUI({
    
    # Get filetype
    filetype <- obtain_filetype(fileloc())
    
    # Render UI if a certain filetype is selected
    if (any(filetype == c("xlsx", "xls")))
      numericInput(inputId = "option_sheetindex", label = "Select Excel Sheet Index", 
                    value = 1)
  })
  
  # ------ HELPER STUFF ------ #
  
  output$tableprint <- renderPrint({
    if (!is.null(input$file)) {
      inFile <- input$file
      
      # Rename the filename to be the same as before
      command1 <- paste0("mv ", inFile$datapath,
                         " ",
                         dirname(inFile$datapath), "/",inFile$name)
      system(command1)
      
      input$option_header
      input$option_sep
      input$option_dec
    }
  })
  
  # ------ AFTER PRESSING "DONE" ------ #
  
  # Do these things when pressing "done" button
  observeEvent(input$done, {
    
    # Assign the dataset to the global environment, 
    # if name of dataset is specified
    if (nzchar(input$name_dataset))
      assign(input$name_dataset, value = dataset(), 
             envir = .GlobalEnv)
    
    # ... and then stop the app
    stopApp()
  })
})

viewer <- dialogViewer(dialogName = "GREA",
                       height = 350,
                       width = 500)

runGadget(ui, server, viewer = viewer)

