#' File-in-Working Directory checker
#'
#' Function to determine if a file lies in a working directory.
#' @param filelocation A character string specifying a file location.
#' @return A file location. Gives the file location without the working directory if the file is in it, but returns the full path if not.
#' @export
#' @examples
#' wd_check("/Users/stani/GitHub/GREA/data/csv_food_supply.csv")

wd_check <- function(filelocation) {
  if (is.null(filelocation) || filelocation == "") {
    return("")
  }
  file <- normalizePath(filelocation, winslash = "/", mustWork = FALSE)
  if (identical(dirname(file), getwd())) {
    gsub("\\\\", "/", basename(filelocation))
  } else {
    gsub("\\\\", "/", filelocation)
  }
}
