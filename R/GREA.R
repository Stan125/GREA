#------------------------------------------------------------------#
# R-Projekt: Gotta Read Em All (GREA)
# Authors: Stanislaus Stadlmann
# Module: Statistical Programming with R (University of Göttingen)
#------------------------------------------------------------------#
#' Display Add-In
#'
#' Display's the GREA Add-In and assigns the right DF to the global environment after pressing "done"
#' @export
#' @import shiny
#' @import miniUI
#' @importFrom tools file_ext
#'
GREA <- function() {

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
                   # Output  '... selected'
                   strong(textOutput("text_selected"), style = "color:green"),
                   p(),
                   # Action button to call fileChoose()
                   actionButton("file_button", "Select File", width = "50%", icon("file"),
                                style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                   p(),
                   # Name of Dataset
                   textInput("name_dataset", label = "Give your dataset a name", width = "90%")
            ),
            column(width = 8,
                   strong(p("Further options (per filetype)", options = "align = top")),
                   # Output for options
                   uiOutput("options")
            )
          ))),

      # Preview Tab Panel
      miniTabPanel(
        "Preview", icon = icon("table"),
        miniContentPanel(
          DT::dataTableOutput("preview")
        ))
    )
  )


  server <- shinyServer(function(input, output, session) {

    # ------ GETTING FILE LOCATION AFTER PRESSING BUTTON ------ #
    fileloc <- reactive({
      # If nothing happens, have NULL
      fp <- NULL

      # If button is pressed, assign the right file loc
      if (input$file_button)
        fp <- fileChoose()

      # If the value is not NULL and the file actually exists, use it
      if (!is.null(fp) && file.exists(fp))
        fp
    })

    # ------ VALIDATION ------ #

    # Creating the dataset
    dataset <- reactive({

      # First try generating the dataset, if something goes wrong display error message
      validate(
        need(
          try(data <- GREA_read(fileloc(),
                                # Options for raw, csv, txt, asc, dat
                                header = input$option_header,
                                sep = input$option_sep, dec = input$option_dec,
                                sheetIndex = input$option_sheetindex)
          ), "        1. Wrong options for generating df, or\n
        2. Outcome is not a df (for Excel and SPSS reader functions)"
       )
      )
      data
    })

    # ------ TEXT: 'blabla.filetype' DETECTED ------ #

    output$text_selected <- renderText({
      if (!is.null(fileloc()))
        # Paste file name after it is selected
        paste0(basename(fileloc()), " selected")
      else
        paste0("Select your file!")
    })

    # ------ PREVIEW TABLE ------ #

    # Render preview table
    output$preview <- DT::renderDataTable({
      dataset()
    }, options = list(autoWidth = FALSE, paging = TRUE, searching = FALSE,
                      info = FALSE, ordering = FALSE, processing = FALSE, scrollX = TRUE),
    class = "cell-border stripe")

    # ------ INTERACTIVE UI ------ #

    observeEvent(input$file_button, {

      # Get filetype
      filetype <- tools::file_ext(fileloc())

      # Render text file UI
      if (any(filetype == c("raw", "csv", "txt", "asc", "dat")))
        output$options <- renderUI({
          list(
            # Column Separator Option
            selectInput(inputId = "option_sep",
                        label = "Select Separator",
                        choices = c("White Space" = " ", "Semicolon" = ";", "Tabstopp" = "\t",
                                    "Dot" = ".", "Comma" = ",")),

            # Decimal Separator Option
            selectInput(inputId = "option_dec",
                        label = "Select Decimal Mark",
                        choices = c("Dot" = ".", "Comma" = ",")),

            # Header option
            checkboxInput(inputId = "option_header", label = "Display header")
          )
        })

      # Excel Options
      else if (any(filetype == c("xls", "xlsx")))
        output$options <- renderUI({
          numericInput(inputId = "option_sheetindex", label = "Select Excel Sheet Index",
                       value = 1)
        })

      # Else nothing
      else
        output$options <- renderUI({ p("") })

    })

    # ------ AFTER PRESSING "DONE" ------ #

    observeEvent(input$done, {

      #### Task 1 ####
      # Check if file is inside the working directory
      file <- wd_check(fileloc())

      # Paste Code into Console
      if (nzchar(fileloc()) && nzchar(input$name_dataset) && !is.null(dataset())) {
        code <- paste0(input$name_dataset, " <- ", GREA_read(filelocation = file, string = TRUE,
                                                             header = input$option_header,
                                                             sep = input$option_sep, dec = input$option_dec,
                                                             sheetIndex = input$option_sheetindex))
        # Paste text into console
        rstudioapi::insertText(text = code, id = "#console")
      }

      #### Task 2 ####
      # ... and then stop the app
      stopApp()
    })

    # ------ WHEN PRESSING "CANCEL" ------ #

    observeEvent(input$cancel, {
      stopApp()
    })
  })

  app <- shinyApp(ui = ui, server = server)
  viewer <- dialogViewer(dialogName = "GREA",
                         height = 350,
                         width = 500)

  runGadget(app, viewer = viewer, stopOnCancel = FALSE)
}
