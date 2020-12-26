#' Add nodes
#'
#' @param dag
#' @param name
#' @param options
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
#' @rdname add_node
add_node <- function(dag, name, options = NULL, x = NULL, y = NULL,
                     right_of = NULL, left_of = NULL,
                     above = NULL, below = NULL, is_swig = FALSE,
                     adorn_math = NULL,
                     ...) {

  if ((!is.null(x) & is.null(y)) | (!is.null(y) & is.null(x))) {
    stop("Must specify both of x and y, or neither")
  }

  id <- count_nodes(dag) + 1

  coords <- get_node_coords(
    dag,
    id,
    coords = c(x, y),
    right_of = right_of,
    left_of = left_of,
    above = above,
    below = below
  )

  if (is.null(c(x,y))) {
    position <- get_node_position(
      dag,
      id,
      right_of = right_of,
      left_of = left_of,
      above = above,
      below = below
    )
  } else position <- NULL


  add_node_to_dag(
    dag = dag,
    name = as.character(name), # so not a problem when getting id
    id = id,
    coords = coords,
    position = position,
    options = options,
    is_swig = is_swig,
    adorn_math = adorn_math
  )
}

#' @export
#'
#' @examples
#' dagtex(node_options = list(shape = "ellipse"),
#'        swig_options = list(gap = "3pt", line_color_right = "red")) %>%
#'  add_many_nodes(list(c("$A_0$", "$a_0$"), "$L_1$", c("$A_1$", "$a_1$"), "$Y$")) %>%
#'  add_many_edges(from = "$a_0$", to = c("$L_1$","$A_1$", "$Y$")) %>%
#'  add_many_edges(from = "$L_1$", to = c("$A_1$", "$Y$")) %>%
#'  add_edge(from = "$a_1$", to = "$Y$")

#' @rdname add_node
add_many_nodes <- function(dag, names, options = NULL, ...) {

  for (i in seq_along(names)) {
    args <- c(dag = list(dag), name = list(names[[i]]),
              is_swig = length(names[[i]]) > 1,
              options = list(options), ...)
    dag <- do.call(add_node, args)
  }

  dag
}

#' @export
#' @rdname add_node
add_swig_node <- function(dag, left, right, options = NULL,  ...) {
  add_node(dag, name = c(left, right), options = options, is_swig = TRUE, ...)
}

any_swig_nodes <- function(dag) {
  any(purrr::map_lgl(dag$nodes, ~.x$is_swig))
}

add_node_to_dag <- function(dag, name, id, coords, position, options, is_swig = FALSE, adorn_math = NULL, ...) {

  node <- structure(
    list(
      name = name,
      id = id,
      coords = coords,
      position = position,
      is_swig = is_swig,
      adorn_math = adorn_math,
      options = options
    ),
    class = "dagtex_node"
  )

  dag$nodes[[id]] <- node

  dag
}

get_node_position <- function(dag, id, right_of = NULL,
                              left_of = NULL, above = NULL, below = NULL) {
  # don't set position if this is the first node
  if (id == 1) return(NULL)

  positions <- c(right_of %||% NA, left_of %||% NA, above %||% NA, below %||% NA)
  positions_not_na <- purrr::map_lgl(positions, ~!is.na(.x))

  if (any(positions_not_na)) {
    location <- c("right", "left", "above", "below")[positions_not_na]

    next_to <- positions[positions_not_na] %>%
      unique() %>%
      purrr::map_dbl(~ifelse(is.character(.x), get_id(dag, .x), .x))

    position <- paste0(
      location,
      "=of ",
      next_to
    )
    return(position)
  }

  # by default, place to the right of previous node
  next_to <- id - 1
  position <- paste("right=of", next_to, ",")

  position
}

# get_node_coords <- function(x, y) paste0("(", x, ",", y, ")")

get_node_coords <- function(dag, id, coords, right_of = NULL,
                            left_of = NULL, above = NULL, below = NULL) {

  if (is.null(coords) & id == 1) return(c(0,0))

  positions <- c(right_of %||% NA, left_of %||% NA, above %||% NA, below %||% NA)

  if (!is.null(right_of) & !is.null(left_of)) {
    warning("Cannot place nodes both left and right of another node. Choosing one.")
  }

  if (!is.null(above) & !is.null(below)) {
    warning("Cannot place nodes both above and below another node. Choosing one.")
  }

  to_add <- purrr::map_dbl(positions, get_id, dag = dag)

  if (!is.null(coords)) {
    if (!all(is.na(to_add))) {
      warning("Supplied coordinates as well as relative position. Choosing coordinates.")
    }
    return(coords)
  }

  if (!all(is.na(to_add))) {
    to_add <- to_add[!is.na(to_add)]

    if (length(to_add) > 1) {
      if (!all(purrr::map_lgl(to_add[2:length(to_add)], identical, to_add[1]))) {
        warning("Cannot place nodes relative to > 1 other node. Choosing one.")
      }
    }

    to_add <- to_add[1]

    vals_h <- c(2, -2)[!is.na(positions[1:2])] %0% 0
    vals_v <- c(2, -2)[!is.na(positions[3:4])] %0% 0

    new_coords <- dag$nodes[[to_add]]$coords + c(vals_h, vals_v)

    return(new_coords)
  }

  c((id - 1)*2, 0)

}

count_nodes <- function(dag) length(dag$nodes)
