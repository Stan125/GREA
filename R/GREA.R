#------------------------------------------------------------------#
# R-Projekt: Gotta Read Em All (GREA)
# Authors: Stanislaus Stadlmann
# Module: Statistical Programming with R (University of Göttingen)
#------------------------------------------------------------------#
#' Display Add-In
#'
#' Display's the GREA Add-In and assigns the right DF to the global environment after pressing "done".
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
                   # Name of Dataset (optional)
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
        )),
      # Advanced Options Tab Panel
      miniTabPanel(
        "Advanced Options", icon = icon("cog"),
        miniContentPanel(
          fillRow(
            # Encoding and Row Skipping
            column(width = 8,
                   uiOutput("adv_options")
            ),
            # NA values
            column(width = 8,
                   textInput("NA_ops", label = "NA values", value = "", width = "90%")
            )
          )
        ))
    )
  )


  server <- shinyServer(function(input, output, session) {

    # ------ OBTAIN FILE LOC ------ #
    fileloc <- reactive({
      # If nothing happens, have NULL
      fp <- NULL

      # If button is pressed, assign the right file loc
      if (input$file_button)
        fp <- fileChoose()

      # If the value is not NULL and exists, use it
      if (!is.null(fp) && file.exists(fp))
        fp

    })

    # ------ UPDATE DATASET NAME ------ #

    observeEvent(input$file_button, {
      if (!is.null(fileloc()))
        # Change Text Input to file name
        updateTextInput(session, "name_dataset", value = tools::file_path_sans_ext(basename(fileloc())))
    })

    # ------ VALIDATION ------ #

    # Creating the dataset
    dataset <- reactive({

      # First try generating the dataset, if something goes wrong display error message
      validate(
        need(
          try({
            # Assemble call
            call <-  quote(GREA_read(fileloc(),
                                     # Options for raw, csv, txt, asc, dat
                                     header = input$option_header,
                                     sep = sep(), dec = input$option_dec,
                                     sheetIndex = input$option_sheetindex))

            if (!is.null(input$NA_ops) && input$NA_ops != "")
              call$na.values <- input$NA_ops
            if (input$encode_ops != "unknown" && !is.null(input$encode_ops))
              call$encoding <- input$encode_ops
            if (input$skip_ops > 0 && !is.null(input$skip_ops))
              call$skip <- input$skip_ops

            data <- eval(call)

          }), "        1. Wrong options for generating df, or\n
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

    # ------ INTERACTIVE UI: BASIC OPTIONS ------ #

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

    # ------ INTERACTIVE UI: ADVANCED OPTIONS ------ #

    observeEvent(input$file_button, {

      # Get filetype
      filetype <- tools::file_ext(fileloc())

      # Render text file UI
      if (any(filetype == c("raw", "csv", "txt", "asc", "dat")))
        output$adv_options <- renderUI({
          list(
            textInput("colsep_ops", label = "Column separator", value = "", width = "90%"),
            p(),
            selectInput("encode_ops", label = "Encoding", choices = c("unknown", "UTF-8", "latin1"), width = "90%"),
            p(),
            numericInput("skip_ops", label = "Skip rows:", value = 0, width = "50%")
          )
        })

      # Excel Options
      else if (any(filetype == c("xls", "xlsx")))
        output$adv_options <- renderUI({
          numericInput("skip_ops", label = "Skip rows:", value = 0, width = "50%")
        })

      # Else nothing
      else
        output$adv_options <- renderUI({ p("") })

    })

    # ------ READ FUNCTION OPTIONS ------ #

    sep <- reactive({
      if (!is.null(input$colsep_ops) && input$colsep_ops != "")
        value_sep <- input$colsep_ops
      else
        value_sep <- input$option_sep

      value_sep
    })

    # ------ AFTER PRESSING "DONE" ------ #

    observeEvent(input$done, {

      #### Task 1 ####
      # Check if file is inside the working directory
      file <- wd_check(fileloc())

      # Paste Code into Console
      if (nzchar(fileloc()) && nzchar(input$name_dataset) && !is.null(dataset())) {

        # Get code that was used to read dataset
        expr <- attributes(dataset())$GREAcommand

        # Assemble code
        code <- paste0(input$name_dataset, " <- ", expr)

        # Paste into Console
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
