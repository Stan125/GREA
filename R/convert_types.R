#' Converts data types
#'
#' Converts the character names of the R standards types to types which can be used by (\code{\link[readxl]{read_excel}}).
#' @param colClasses A character vector specifying the classes of the columns. This vector is used by (\code{\link{read.table}}).
#' @param to A string specifying the package to which type names the vector colClasses has to convert.
#' @return A character vector vector specifying the classes of the columns. This vector is used by the package which is specified in to.

convert_types <- function(colClasses, to = "readxl"){
  col_types <- colClasses

  if (to == "readxl")
    col_types <- convert_types_readxl(colClasses)

  col_types
}

convert_types_readxl <- function(colClasses){
  col_types <- colClasses

  col_types <- gsub("character|factor|complex|raw", "text", col_types)
  col_types <- gsub("integer", "numeric", col_types)
  col_types <- gsub("Date|POSIXct", "date", col_types)
  col_types <- gsub("NULL", "skip", col_types)
  col_types[is.na(col_types)] <- "guess"

  col_types
}
