#' File-in-Working Directory checker
#'
#' Function to determine if a file lies in a working directory.
#' @return A file location. Gives the file location without the WD if the file is in the WD, but returns only the input file location if it doesn't.
#' @examples
#' wd_check("/Users/stani/GitHub/GREA/data/csv_food_supply.csv")

wd_check <- function(filelocation) {
  file_split <- strsplit(filelocation, "/")[[1]]
  wd_split <- strsplit(getwd(), "/")[[1]]

  # Return filepaths depending on whether the file is in the WD
  if (all(wd_split %in% file_split)) {
    # Return filepath without WD
    file_in_wd <- paste(file_split[seq(length(wd_split) + 1, length(file_split))], collapse = "/")
    return(file_in_wd)
  } else {
    return(filelocation)
  }
}
