#' Title
#'
#' @param path A path to a file
#'
#' @return A path to a file
#' @export
#'
#' @examples
#' path_to_file("dumbo.jpg")
path_to_file <- function(path = NULL) {
  if (is.null(path)) {
    dir(system.file("extdata", package = "gentr"))
  } else {
    system.file("extdata", path, package = "gentr", mustWork = TRUE)
  }
}
