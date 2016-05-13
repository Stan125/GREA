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
          # File Input
          fileInput("file", label = "Select file to read into R")
        ))),
    
    # Preview Tab Panel
    miniTabPanel(
      "Preview", icon = icon("table"),
      miniContentPanel(
        #DT::dataTableOutput("preview")
        textOutput("tableprint")
      ))
    
  )
)


server <- shinyServer(function(input, output) {
  
  # # Render preview table
  # output$preview <- DT::renderDataTable({
  #   
  #   # Store file
  #   inFile <- input$file
  #   
  #   # Read it, and display it
  #   GREA_fun(inFile$datapath)
  #   
  # }, options = list(autoWidth = FALSE, 
  #                   paging = TRUE,
  #                   searching = FALSE,
  #                   info = TRUE,
  #                   ordering = FALSE,
  #                   processing = FALSE,
  #                   scrollX = TRUE),
  # class = "cell-border stripe")
  
  
  
  # Close when pressing "done" button
  observeEvent(input$done, {
    stopApp()
  })
})

viewer <- dialogViewer(dialogName = "GREA", 
                       height = 300, 
                       width = 500)

runGadget(ui, server, viewer = viewer)

