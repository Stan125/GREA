#' Read Data Wrapper
#'
#' Reads data in any of the formats supported by \code{\link[rio]{import}}.
#' @param filelocation A single character string with the location and name of the file, e.g. \dQuote{data/bla.csv}.
#' @param sheetIndex An integer specifying which sheet to import from an Excel or ODS spreadsheet file.
#' @param sep A character string specifying a column separator for delimited files.
#' @param dec A character string specifying a decimal separator for delimited files.
#' @param header A logical indicating whether a delimited file contains a header row.
#' @return A dataframe, containing the read-in data.
#' @importFrom tools file_ext
#' @importFrom rio import
#' @importFrom R.matlab readMat
#' @export

GREA_read <- function(filelocation, sheetIndex = 1L, sep, dec = "auto", header) {
    if (is.null(filelocation) || filelocation == "") {
      return("")
    }
    filetype <- file_ext(filelocation)
    arg <- NULL
    if (tolower(filetype) == "mat") {
        cmd <- paste0("R.matlab::readMat(con = '", filelocation, "'")
    } else {
        if (tolower(filetype) %in% c("raw", "asc", "dat")) {
            arg <- paste0(arg, ", format = 'txt'")
        }
        if (!missing(sep) && sep != "auto") {
            arg <- paste0(arg, ", sep = '", sep,"'")
        }
        if (!missing(dec) && dec != "auto") {
            arg <- paste0(arg, ", sep2 = '", dec,"'")
        }
        if (!missing(header)) {
            arg <- paste0(arg, ", header = ", header)
        }
        if (tolower(filetype) %in% c("xls", "xlsx")) {
            arg <- paste0(arg, ", which = ", sheetIndex)
        }
        cmd <- paste0("rio::import('", filelocation, "'", arg, ")")
    }
    structure(eval(parse(text = cmd)), GREAcommand = cmd)
}
