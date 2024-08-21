#' Write image
#'
#' @param image An image object
#' @param path A path to save the image to
#'
#' @return NULL
#' @export
#'
#' @examples
#' image <- magick::image_read(path_to_file("dumbo.jpg"))
#' write_image(image, "dumbo.jpg")
write_image <- function(image, path) {
  magick::image_write(image, path)
}

floyd_steinberg_dithering <- function(mat) {
  for (x in 2:nrow(mat) - 1) {
    for (y in 2:ncol(mat) - 1) {
      oldpixel <- mat[x, y]
      newpixel <- round(oldpixel)
      mat[x, y] <- newpixel
      quant_error <- oldpixel - newpixel
      mat[x + 1, y] <- mat[x + 1, y] + quant_error * 7 / 16
      mat[x - 1, y + 1] <- mat[x - 1, y + 1] + quant_error * 3 / 16
      mat[x, y + 1] <- mat[x, y + 1] + quant_error * 5 / 16
      mat[x + 1, y + 1] <- mat[x + 1, y + 1] + quant_error * 1 / 16
    }
  }
  return(mat)
}

#' Floyd dither
#'
#' @param path A path to an image
#'
#' @return An image object
#' @export
#'
#' @examples
#' floyd_dither(path_to_file("dumbo.jpg"))
floyd_dither <- function(path) {
  image <- magick::image_read(path) |>
    magick::image_resize("600x")
  mat <- as.integer(image[[1]])
  mat_gray <- apply(mat, c(1, 2), mean) / 255
  dithered <- array(floyd_steinberg_dithering(mat_gray),
                    dim = c(dim(mat_gray)[1:2], 1))
  magick::image_read(dithered)
}
