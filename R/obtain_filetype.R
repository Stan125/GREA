#' Obtain Filetype
#'
#' Obtains the filetype of a file in a string
#' @param filelocation A single string with the location and name of the file, e.g. "data/bla.csv"
#' @return The filetype, e.g. "csv"
#' @export
obtain_filetype <- function(filelocation) {
  if (is.null(filelocation))
    return(NULL)

  # Split string
  str <- strsplit(filelocation, "[.]")[[1]]

  # File ending
  filetype <- str[length(str)]

  return(filetype)
}

