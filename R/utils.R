`%||%` <- purrr::`%||%`

`%0%` <- function (x, y) {
    if (purrr::is_empty(x)) y else x
}
