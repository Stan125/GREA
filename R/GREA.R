#------------------------------------------------------------------#
# R-Projekt: Gotta Read Em All (GREA)
# Authors: Stanislaus Stadlmann
# Module: Statistical Programming with R (University of Göttingen)
#------------------------------------------------------------------#
#' Display Add-In
#'
#' Display's the GREA Add-In and assigns the right DF to the global
#' environment after pressing "done".
#' @export
#' @import shiny
#' @import miniUI
#' @import rhandsontable
#' @importFrom tools file_ext
#' @importFrom utils head
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
              actionButton("file_button", "Select File", width = "50%",
                icon("file"), style = "color: #fff;
                background-color: #337ab7; border-color: #2e6da4"),
              p(),
              # Name of Dataset (optional)
              textInput("name_dataset", label = "Give your dataset a name",
                width = "90%")
            ),
            column(width = 8,
              strong(p("Further options (per filetype)",
                options = "align = top")),
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

      # Data Types Panel
      miniTabPanel(
        "Data Types", icon = icon("columns"),
        miniContentPanel(
          actionButton("update_button", "Reload names", width = "50%",
            icon("sync"), style = "color: #fff;
            background-color: #337ab7; border-color: #2e6da4"),
          rHandsontableOutput("classes", height = "200px")
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
              textInput("NA_ops", label = "NA values", value = "",
                width = "90%")
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
        updateTextInput(
          session,
          "name_dataset",
          value = base::make.names(
            tools::file_path_sans_ext(basename(fileloc()))
          )
        )
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

            if (!is.null(input$classes)) {
              # parse handsontable to R-object
              column_df <- hot_to_r(input$classes)
              # extract right vec
              col_class_vec <- column_df$column_classes
              # set argument
              call$colClasses <- col_class_vec
            }

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


    # ------ PREVIEW TABLE ------#

    # Render preview table
    output$preview <- DT::renderDataTable({
      dataset()
    }, options = list(autoWidth = FALSE, paging = TRUE, searching = FALSE,
                      info = FALSE, ordering = FALSE, processing = FALSE,
                      scrollX = TRUE),
    class = "cell-border stripe")


    # ------ PREVIEW COLUMN TYPES ------#

    output$classes <- renderRHandsontable({
      # active manual update of isolated data
      input$update_button

      # Isolate data to not active the ping-pong between render and reactive
      data <- isolate(dataset())

      column_names <- colnames(data)
      column_classes <- rep("character", length(column_names))
      column_info <- as.character(
        lapply(head(data, 10), function(x) paste0(x, collapse = ", ")))
      df <- data.frame(column_names, column_classes, column_info,
        stringsAsFactors = FALSE)

      hot <- rhandsontable::rhandsontable(df)
      pos_types <- c("character", "numeric", "integer", "factor", "Date",
        "POSIXct", "logical", "NULL", NA)
      # remove complex and raw
      hot_col(hot, col = "column_classes", type = "dropdown",
        source = pos_types, strict = TRUE)
    })

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
                        choices = c("White Space" = " ", "Semicolon" = ";",
                          "Tabstopp" = "\t", "Dot" = ".", "Comma" = ",")),

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
          numericInput(inputId = "option_sheetindex",
            label = "Select Excel Sheet Index",
                       value = 1)
        })

      # Else nothing
      else
        output$options <- renderUI(shiny::p(""))

    })

    # ------ INTERACTIVE UI: ADVANCED OPTIONS ------ #

    observeEvent(input$file_button, {

      # Get filetype
      filetype <- tools::file_ext(fileloc())

      # Render text file UI
      if (any(filetype == c("raw", "csv", "txt", "asc", "dat")))
        output$adv_options <- renderUI({
          list(
            textInput("colsep_ops", label = "Column separator", value = "",
              width = "90%"),
            p(),
            selectInput("encode_ops", label = "Encoding",
              choices = c("unknown", "UTF-8", "latin1"), width = "90%"),
            p(),
            numericInput("skip_ops", label = "Skip rows:", value = 0,
              width = "50%"),
            p(),
            checkboxInput("fread_ops", "Let ", FALSE),

          )
        })

      # Excel Options
      else if (any(filetype == c("xls", "xlsx")))
        output$adv_options <- renderUI({
          shiny::numericInput("skip_ops", label = "Skip rows:", value = 0,
            width = "50%")
        })

      # Else nothing
      else
        output$adv_options <- renderUI(p(""))

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
      # Paste Code into Console
      if (nzchar(fileloc()) &&
          nzchar(input$name_dataset) &&
          !is.null(dataset())) {

        # Get code that was used to read dataset
        expr <- attributes(dataset())$GREAcommand

        code <- paste0(base::make.names(input$name_dataset), " <- ", expr)

        # Paste into Console
        rstudioapi::insertText(text = code, id = "#console")
      }

      #### Task 2 ####
      # ... and then stop the app
      shiny::stopApp()
    })

    # ------ WHEN PRESSING "CANCEL" ------ #

    observeEvent(input$cancel, {
      shiny::stopApp()
    })
  })

  app <- shiny::shinyApp(ui = ui, server = server)
  viewer <- dialogViewer(dialogName = "GREA",
                         height = 350,
                         width = 500)

  shiny::runGadget(app, viewer = viewer, stopOnCancel = FALSE)
}
