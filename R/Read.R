#' Read Data Wrapper
#'
#' Reads Data of many different formats. Currently: .dta (STATA), .sav (SPSS), .mat (MATLAB), .xls/.xlsx (Excel), and .raw, .csv, .txt, .asc, .dat. Is the basis-function for the GREA add-in.
#' @param filelocation A single string with the location and name of the file, e.g. \dQuote{data/blabla.csv}
#' @param header A logical indicating whether a text-delimited file contains a header row.
#' @param sep A character string specifying a column separator.
#' @param dec A character string specifying a decimal separator.
#' @param colClasses A character vector specifying the classes of the columns. This vector is used by (\code{\link{read.table}}).
#' @param col_types A character vector specifying the classes of the columns. This vector is used by (\code{\link[readxl]{read_excel}}).
#' @param sheetIndex A numerical value to indicate which sheet to import (Excel formats).
#' @param na.values A character string specifying which values to convert to NA's.
#' while importing. Can be a vector of values when reading text-delimited files (\code{\link{read.table}}),
#' and should be a single value when importing Excel files (\code{\link[readxl]{read_excel}}).
#' @param skip How many rows should be skipped? Only for text-delimited files and Excel files.
#' @param encoding Encoding of text-delimited files (see \code{\link{read.table}})
#' @return A dataframe, containing the read-in data plus the code to read the data as an attribute.
#' @importFrom tools file_ext
#' @export


## Function: GREA_read
GREA_read <- function(filelocation, header = FALSE, sep = " ", dec = ".", colClasses = NA,
                      col_types = TRUE, sheetIndex = 1, na.values, skip = 0, encoding = "unknown") {

    if (is.null(filelocation))
      return(NULL)

    # Obtain filetype, add tolerance for uppercase extensions
    filetype <- tolower(file_ext(filelocation))

    # Fix the filelocation string (because of very annoying windows bug)
    filelocation <- wd_check(filelocation)

    # ------ Text-like Files with options ------ #

    # raw, csv, txt, asc, dat
    if (any(filetype == c("raw", "csv", "txt", "asc", "dat"))) {
      expr <- quote(read.table())
      expr[c("file", "sep", "dec", "header", "colClasses")] <- list(filelocation, sep, dec, header, colClasses)

      # Extra options
      if (!missing(na.values))
        expr[c("na.strings")] <- na.values
      if (skip > 0)
        expr[c("skip")] <- skip
      if (encoding != "unknown")
        expr[c("fileEncoding")] <- encoding
    }

    # ------ Non-Text files with "which" options ------ #

    # Excel: .xls, .xlsx
    else if (any(filetype == c("xls", "xlsx"))) {
      expr <- quote(rio::import())
      expr[c("file", "which", "col_types")] <- list(filelocation, sheetIndex, convert_types(colClasses))
      if (!missing(na.values))
        expr[c("na")] <- na.values
      if (skip > 0)
        expr[c("skip")] <- skip
    }

    # ------ Files unknown to rio ------ #

    # MATLAB: .mat
    else if (filetype == "mat") {
      expr <- quote(R.matlab::readMat())
      expr["con"] <- filelocation
    }

    # ------ Non-Text files without options ------ #

    else {
      expr <- quote(rio::import())
      expr[c("file")] <- list(filelocation)
      if (!missing(na.values))
        expr[c("na.strings")] <- na.values
    }

    # Give back DF and attach command to it
      return(structure(eval(expr), GREAcommand = deparse(expr, width.cutoff = 500L)))
  }
