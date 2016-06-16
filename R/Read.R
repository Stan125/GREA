#' Read Data Wrapper
#'
#' Reads Data of many different formats. Currently: .dta (STATA), .sav (SPSS), .mat (MATLAB), .xls/.xlsx (Excel), and .raw, .csv, .txt, .asc, .dat. Is the basis-function for the GREA add-in
#' @param filelocation A single string with the location and name of the file, e.g. "data/bla.csv"
#' @param header Should the header be read in?
#' @param sep
#' @param dec
#' @param into.dataframe
#' @param sheetIndex
#' @return A dataframe, containing the read-in data
#' @importFrom tools file_ext
#' @export

## Function: GREA_read
GREA_read <- function(filelocation, header = FALSE, sep = " ", dec = ".",
                     into.dataframe = TRUE, sheetIndex = 1, string = FALSE) {

  # Wrap TryCatch around to specify error messages
  tryCatch({

    if (is.null(filelocation))
      return(NULL)

    # Obtain filetype, add tolerance for big extensions
    filetype <- tolower(file_ext(filelocation))

    # Fix the filelocation string (because of very annoying windows bug)
    filelocation <- wd_check(filelocation)

    # ------ Text-like Files with options ------ #

    # raw, csv, txt, asc, dat
    if (any(filetype == c("raw", "csv", "txt", "asc", "dat")))
      expr <- paste0("read.table(file = ", "'", filelocation, "', ",
                     "header = ", header, ", ", "sep = ", "'",  sep, "'",", dec = ", "'", dec, "')")

    # ------ Non-Text files with options ------ #

    # Excel: .xls, .xlsx
    else if (any(filetype == c("xls", "xlsx")))
      expr <- paste0("rio::import(file = ", "'", filelocation, "', ", "which = ", sheetIndex, ")")

    # ------ Matlab Files ------ #

    # MATLAB: .mat
    else if (filetype == "mat")
      expr <- paste0("R.matlab::readMat(con = ", "'", filelocation, "'", ")")

    # ------ Non-Text files without options ------ #

    else
      expr <- paste0("rio::import(", "'", filelocation, "')")

    # Give back DF
    if (string == FALSE)
      return(structure(eval(parse(text = expr)), GREAcommand = expr))
    else if (string == TRUE)
      return(expr)
  }
  ,

  # ------ Files with other options ------ #
  error = function(err) {
    message("1. Wrong options for generating df, or")
    message("2. Outcome is not a df (for Excel and SPSS reader functions)")
  },
  warning = function(war) {
    message("1. Wrong options for generating df, or")
    message("2. Outcome is not a df (for Excel and SPSS reader functions)")
  }
  )
}
