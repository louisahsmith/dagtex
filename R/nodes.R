#' Add nodes
#'
#' @param .dag
#' @param .name
#' @param .options
#' @param x
#' @param y
#' @param right_of
#' @param left_of
#' @param above
#' @param below
#' @param is_swig
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'
#' @rdname add_nodes
add_node <- function(.dag, .name, .options = NULL, x = NULL, y = NULL,
                     right_of = NULL, left_of = NULL,
                     above = NULL, below = NULL, is_swig = FALSE,
                     ...) {

  id <- count_nodes(.dag) + 1

  coords <- get_node_coords(
    x = x,
    y = y
  )

  position <- get_node_position(
    .dag,
    id,
    coords,
    right_of = right_of,
    left_of = left_of,
    above = above,
    below = below
  )

  add_node_to_dag(
    .dag = .dag,
    .name = .name,
    .id = id,
    .coords = coords,
    .position = position,
    .options = .options,
    is_swig = is_swig
  )
}

#' @export
#'
#' @examples
#' dagtex(.node_options = list(shape = "ellipse"),
#'        .swig_options = list(gap = "3pt", line_color_right = "red")) %>%
#'  add_nodes(list(c("$A_0$", "$a_0$"), "$L_1$", c("$A_1$", "$a_1$"), "$Y$")) %>%
#'  add_edges(.from = "$a_0$", .to = c("$L_1$","$A_1$", "$Y$")) %>%
#'  add_edges(.from = "$L_1$", .to = c("$A_1$", "$Y$")) %>%
#'  add_edge(.from = "$a_1$", .to = "$Y$")

#' @rdname add_nodes
add_nodes <- function(.dag, .names, .options = NULL, ...) {

  for (i in seq_along(.names)) {
    args <- c(.dag = list(.dag), .name = list(.names[[i]]),
              is_swig = length(.names[[i]]) > 1,
              .options = .options)
    .dag <- do.call(add_node, args)
  }

  .dag
}

#' @export
#' @rdname add_nodes
add_swig_node <- function(.dag, .left, .right, .options = NULL, x = NULL, y = NULL,
                          right_of = NULL, left_of = NULL,
                          above = NULL, below = NULL, ...) {
  add_node(.dag, .name = c(.left, .right), .options = .options, x = x, y = y,
           right_of = right_of, left_of = left_of,
           above = above, below = below, is_swig = TRUE, ...)
}

any_swig_nodes <- function(.dag) {
  any(purrr::map_lgl(.dag$nodes, ~.x$is_swig))
}

add_node_to_dag <- function(.dag, .name, .id, .coords, .position, .options, is_swig = FALSE) {

  node <- structure(
    list(
      name = .name,
      id = .id,
      coords = .coords,
      position = .position,
      is_swig = is_swig,
      options = .options
    ),
    class = "dagtex_node"
  )

  .dag$nodes[[.id]] <- node

  .dag
}

get_node_position <- function(.dag, .id, coords, right_of = NULL,
                              left_of = NULL, above = NULL, below = NULL) {
  # don't set position if coordinates are given or if this is the first node
  if (!is.null(coords) | .id == 1) return(NULL)

  positions <- c(right_of %||% NA, left_of %||% NA, above %||% NA, below %||% NA)
  positions_not_na <- purrr::map_lgl(positions, ~!is.na(.x))

  if (any(positions_not_na)) {
    location <- c("right", "left", "above", "below")[positions_not_na]

    next_to <- positions[positions_not_na] %>%
      unique() %>%
      purrr::map_dbl(~ifelse(is.character(.x), get_id(.dag, .x), .x))

    position <- paste(
      location,
      "=of",
      next_to
    )
    return(position)
  }

  # by default, place to the right of previous node
  next_to <- .id - 1
  position <- paste("right =of", next_to, ",")

  position
}

get_node_coords <- function(x = NULL, y = NULL) {
  if (!is.null(x) & !is.null(x)) return(paste0("(", x, ",", y, ")"))

  NULL
}

count_nodes <- function(.dag) length(.dag$nodes)

last_node <- function() {
  function(.dag) {
    previous_node <- count_nodes(.dag) - 1

    if (previous_node == 0) {
      warning("No previous node: returning `NA`")
      return(NA)
    }

    previous_node
  }
}

