#' @keywords internal
`%||%` <- purrr::`%||%`

#' @keywords internal
`%0%` <- function(x, y) {
    if (purrr::is_empty(x)) y else x
}


#' for building pkgdown site with pics
#' @keywords internal
replay_html.magick <- function(x, fig_save, fig_id, ...) {

  # just to get path info, will write over
   fig <- fig_save(x, fig_id())
   magick::image_write(x, path = fig$path, format = "png")

  paste0(
    "<div class='img'>",
    "<img src='", downlit:::escape_html(fig$path),
    # "' alt='' width='", fig$width, "' height='", fig$width,
    "' />",
    "</div>"
  )
 }
