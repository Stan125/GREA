#' File-in-Working Directory checker
#'
#' Function to determine if a file lies in a working directory.
#' @param A character string specifying a file location
#' @return A file location. Gives the file location without the working directory if the file is in it (also can be a subfolder), but returns the full path if not.
#' @export
#' @examples
#' filepath_test <- file.path(getwd(), "data", "testfile.csv")
#' wd_check(filepath_test)

wd_check <- function(filelocation) {

  # If null return NULL
  if (is.null(filelocation) || filelocation == "") {
    return("")
  }

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
