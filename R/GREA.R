#------------------------------------------------------------------#
# R-Projekt: Gotta Read Em All (GREA)
# Authors: Stanislaus Stadlmann
# Module: Statistical Programming with R (University of Göttingen)
#------------------------------------------------------------------#
#' Display Add-In
#'
#' Display's the GREA Add-In and assigns the right DF to the global environment after pressing "done"
#' @export
#' @import rio
#' @import shiny
#' @import miniUI
#'
GREA <- function() {

  # Set max File size
  options(shiny.maxRequestSize = 500*1024^2)

  ui <- miniPage(
    gadgetTitleBar(title = "Gotta Read 'Em All"),
    miniTabstripPanel(
      # Input Tab Panel
      miniTabPanel(
        "Input", icon = icon("folder-open"),
        miniContentPanel(
          # Action button to call fileChoose()
          actionButton("file_button", "Select File", width = "50%", icon("file"),
                       style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
          uiOutput("fileselected"),
          p(""),
          textInput("name_dataset", label = "Dataset name (optional)", value = "", width = "90%"),
          uiOutput("fileoptions") # options displayed dynamically
        ),
        value = "Input"
      ),
      # Preview Tab Panel
      miniTabPanel(
        "Preview", icon = icon("table"),
        miniContentPanel(
          DT::dataTableOutput("previewtable")
        ),
        value = "Preview"
      ),
      id = "tabStripPanel",
      selected = "Input"
    )
  )


  server <- shinyServer(function(input, output, session) {

    # define reactive elements
    fileloc <- eventReactive(input$file_button, {
      wd_check(fileChoose())
    })
    dataset <- reactive({
      if (is.null(input$option_separator)) {
        if (is.null(input$option_header)) {
          GREA_read(fileloc(), sheetIndex = input$option_sheetindex)
        } else {
          GREA_read(fileloc(), sheetIndex = input$option_sheetindex, 
                               header = input$option_header)
        }
      } else {
        if (is.null(input$option_header)) {
          GREA_read(fileloc(), sheetIndex = input$option_sheetindex, 
                               sep = input$option_separator)
        } else {
          GREA_read(fileloc(), sheetIndex = input$option_sheetindex, 
                               sep = input$option_separator, 
                               header = input$option_header)
        }
        
      }
    })
    dataname <- reactive({
        if (input$name_dataset == "") {
            tools::file_path_sans_ext(basename(fileloc()))
        } else {
            input$name_dataset
        }
    })
    
    # observe file selection
    observeEvent(input$file_button, {
      if (!is.null(fileloc()) && fileloc() != "") {
        output$fileselected <- renderUI({
          p(strong("File selected: "), fileloc())
        })
        .txtformats <- c("txt", "tsv", "csv", "csv2", "raw", "asc", "dat", "psv")
        if (tolower(file_ext(fileloc())) %in% c("xls", "xlsx")) {
          # display Excel options
          output$fileoptions <- renderUI({
            numericInput(inputId = "option_sheetindex", 
                         label = "Select Excel Sheet Index",
                         value = 1)
          })
        } else if (tolower(file_ext(fileloc())) %in% .txtformats) {
          # display delimited file options
          output$fileoptions <- renderUI({
            list(
              textInput(inputId = "option_separator", 
                        label = "Column separator",
                        value = "auto"),
              selectInput(inputId = "option_dec",
                          label = "Decimal Mark",
                          choices = c("Auto" = "auto", "Dot (.)" = ".", "Comma (,)" = ",")),
              checkboxInput(inputId = "option_header",
                            label = strong("Header"),
                            value = TRUE)
            )
          })
        } else {
          # otherwise display nothing
          output$fileoptions <- renderUI({ p("") })
        }
        # render result
        output$previewtable <- DT::renderDataTable(dataset(), 
          options = list(autoWidth = FALSE, paging = TRUE, searching = FALSE, info = FALSE, 
                       ordering = FALSE, processing = FALSE, scrollX = TRUE),
          class = "cell-border stripe")
      }
    })
    
    # observe "done" button
    observeEvent(input$done, {
      if (is.null(fileloc()) || fileloc() == "") {
        stopApp()
      }
      if (!is.null(dataset)) {
        cmd <- attributes(dataset())$GREAcommand
        if (!is.null(cmd)) {
          rstudioapi::insertText(text = paste0(dataname(), " <- ", cmd), id = "#console")
        }
      }
      
      stopApp()
    })

  })
  
  app <- shinyApp(ui = ui, server = server)
  viewer <- dialogViewer(dialogName = "GREA", height = 400, width = 500)
  runGadget(app, viewer = viewer, stopOnCancel = TRUE)
}

