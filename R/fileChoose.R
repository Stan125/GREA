#' Better File Chooser
#'
#' Builds upon file.choose(), but returns NULL when cancelled
#' @return The location of the file
#' @export

fileChoose <- function() {
  pathname <- NULL
  tryCatch({
    pathname <- file.choose()
  }, error = function(ex) {
    ""
  })
  pathname
}
