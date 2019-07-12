#' Converts data types
#'
#' Converts the character names of the R standards types to types which can be used by (\code{\link[readxl]{read_excel}}).
#' @param colClasses A character vector specifying the classes of the columns. This vector is used by (\code{\link{read.table}}).
#' @return A character vector vector specifying the classes of the columns. This vector is used by (\code{\link[readxl]{read_excel}}).

convert_types <- function(colClasses){
  col_types <- colClasses
  col_types <- gsub("character|factor|complex|raw", "text", col_types)
  col_types <- gsub("integer", "numeric", col_types)
  col_types <- gsub("Date|POSIXct", "date", col_types)
  col_types <- gsub("NULL", "skip", col_types)
  col_types[is.na(col_types)] <- "guess"

  col_types
}
