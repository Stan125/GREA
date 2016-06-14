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
                                into.dataframe = input$option_todf,
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

    # Render the UI button with sep selections, if dataset is selected
    output$sep <- renderUI({
      # Get filetype
      filetype <- obtain_filetype(fileloc())

      # Render UI if a certain filetype is selected
      if (any(filetype == c("raw", "csv", "txt", "asc", "dat")))
        selectInput(inputId = "option_sep",
                    label = "Select Separator",
                    choices = c("White Space" = " ", "Semicolon" = ";", "Tabstopp" = "\t",
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

    # output$tableprint <- renderPrint({
    #   if (!is.null(input$file)) {
    #     inFile <- input$file
    #
    #     # Rename the filename to be the same as before
    #     command1 <- paste0("mv ", inFile$datapath,
    #                        " ",
    #                        dirname(inFile$datapath), "/",inFile$name)
    #     system(command1)
    #
    #     input$option_header
    #     input$option_sep
    #     input$option_dec
    #   }
    # })

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
                                                             into.dataframe = input$option_todf,
                                                             sheetIndex = input$option_sheetindex))
        # Paste text into console
        rstudioapi::insertText(text = code, id = "#console")
      }

      #### Task 2 ####
      # ... and then stop the app
      invisible(stopApp())
    })

    # ------ WHEN PRESSING "CANCEL" ------ #

    observeEvent(input$cancel, {
      invisible(stopApp())
    })
  })

  app <- shinyApp(ui = ui, server = server)
  viewer <- dialogViewer(dialogName = "GREA",
                         height = 350,
                         width = 500)

  runGadget(app, viewer = viewer, stopOnCancel = FALSE)
}
