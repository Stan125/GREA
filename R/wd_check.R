#' File-in-Working Directory checker
#'
#' Function to determine if a file lies in a working directory.
#' @return A file location. Gives the file location without the WD if the file is in the WD, but returns only the input file location if it doesn't.
#' @export
#' @examples
#' wd_check("/Users/stani/GitHub/GREA/data/csv_food_supply.csv")

wd_check <- function(filelocation) {
  # deconstruct file path depending on platform
  if (grepl("[\\]", filelocation)) {
    file_split <- unlist(strsplit(filelocation, "[\\]"))
    wd_split <- unlist(strsplit(getwd(), "[\\]"))
  } else if (grepl("/", filelocation)) {
    file_split <- unlist(strsplit(filelocation, "/"))
    wd_split <- unlist(strsplit(getwd(), "/"))
  }

  # Return filepaths depending on whether the file is in the WD
  if (all(wd_split %in% file_split)) {
    # Return filepath without WD
    file_in_wd <- file_split[seq(length(wd_split) + 1, length(file_split))]
    file_in_wd <- do.call("file.path", as.list(file_in_wd))
    return(file_in_wd)
  } else {
    file <- do.call("file.path", as.list(file_split))
    return(file)
  }
}
