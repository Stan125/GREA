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
  gadgetTitleBar(
    # Name that is displayed
    title = "Gotta Read 'Em All"
  ),
  
  # 2 Rows
  fillRow(
    miniContentPanel(
      # File Input
      fileInput("file", label = "Select file to read into R")
    )
  )
)


server <- shinyServer(function(input, output) {
  # Close when pressing "done" button
  observeEvent(input$done, {
    stopApp()
  })
})

viewer <- dialogViewer(dialogName = "GREA", 
                       height = 300, 
                       width = 500)

runGadget(ui, server, viewer = viewer)

