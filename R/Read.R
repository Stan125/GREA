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
#' @export

## Function: GREA_read
GREA_read <- function(filelocation, header = FALSE, sep = " ", dec = ".",
                     into.dataframe = TRUE, sheetIndex = 1, string = FALSE) {

  # Wrap TryCatch around to specify error messages
  tryCatch({

    if (is.null(filelocation))
      return(NULL)

    filetype <- obtain_filetype(filelocation)

    # ------ Files without any options ------ #

    # STATA: .dta
    if (filetype == "dta")
      expr <- paste0("foreign::read.dta(file = ", "'", filelocation, "')")

    # MATLAB: .mat
    else if (filetype == "mat")
      expr <- paste0("R.matlab::readMat(con = ", "'", filelocation, "'", ")")

    # ------ Files with sep, header, dec, NA options ------ #

    # raw, csv, txt, asc, dat
    else if (any(filetype == c("raw", "csv", "txt", "asc", "dat")))
      expr <- paste0("read.table(file = ", "'", filelocation, "', ",
                     "header = ", header, ", ", "sep = ", "'",  sep, "'",", dec = ", "'", dec, "')")

    # ------ Files with other options ------ #

    # SPSS: .sav
    else if (filetype == "sav")
      expr <- paste0("foreign::read.spss(file = ", "'", filelocation, "', to.data.frame = ", into.dataframe, ")")

    # Excel: .xls, .xlsx
    else if (any(filetype == c("xls", "xlsx")))
      expr <- paste0("readxl::read_excel(path = ", "'", filelocation, "', ", "sheet = ", sheetIndex, ")")

    # Give back DF
    if (string == FALSE)
      return(suppressWarnings(eval(parse(text = expr))))
    else if (string == TRUE)
      return(expr)
  },

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
