#' Add a single node to a DAG
#'
#' @param dag Object of class "dagtex", created with [dagtex()].
#' @param name Name of the node, to be printed in DAG. If names within a DAG are
#'   unique, they can also be used when creating edges. If a split node in a
#'   SWIG (`is_swig = TRUE`), should be a character vector of length two.
#' @param options List of node options specific to this node. For details, see
#'   [dagtex()].
#' @param x Numeric. Horizontal coordinate at which to place node. If provided,
#'   `y` must also be provided.
#' @param y Numeric. Vertical coordinate at which to place node.If provided, `x`
#'   must also be provided.
#' @param right_of Name of node already existing in DAG to place current node to
#'   the right of.
#' @param left_of Name of node already existing in DAG to place current node to
#'   the left of.
#' @param above Name of node already existing in DAG to place current node
#'   above.
#' @param below Name of node already existing in DAG to place current node
#'   below.
#' @param is_swig Logical. If node is to be split as part of a SWIG. See also
#'   [add_swig_node()]. Defaults to `FALSE`.
#' @param adorn_math Logical. Node name will be automatically surrounded by
#'   `$...$` if `TRUE`. Defaults to `FALSE`.
#' @param ... Not in use.
#' @keywords internal
#' @return The DAG
#'
add_single_node <- function(dag, name, options = NULL, x = NULL, y = NULL,
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

  if (is.null(c(x, y))) {
    position <- get_node_position(
      dag,
      id,
      right_of = right_of,
      left_of = left_of,
      above = above,
      below = below
    )
  } else {
    position <- NULL
  }


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

#' Add node(s) to a DAG
#'
#' @param dag Object of class "dagtex", created with [dagtex()].
#' @param names Name of the node, to be printed in DAG.
#'   Can also be a vector of names to add multiple nodes at once, in which case
#'   the first node will be placed at the requested location, and
#'   dagtex() will attempt to place the rest appropriately. For finer control,
#'   add nodes individually. If names within a DAG are
#'   unique, they can also be used when creating edges. If any of the nodes are
#'   split nodes in a SWIG, provide names as a list with nodes to split as
#'   vectors of length 2 (see example).
#' @param options List of node options specific to these nodes. For details, see
#'   [dagtex()].
#' @param x Numeric. Horizontal coordinate at which to place (first) node. If provided,
#'   `y` must also be provided.
#' @param y Numeric. Vertical coordinate at which to place (first) node. If provided, `x`
#'   must also be provided.
#' @param right_of Name of node already existing in DAG to place (first) node to
#'   the right of.
#' @param left_of Name of node already existing in DAG to place (first) node to
#'   the left of.
#' @param above Name of node already existing in DAG to place (first) node
#'   above.
#' @param below Name of node already existing in DAG to place (first) node
#'   below.
#' @param adorn_math Logical. Node names will be automatically surrounded by
#'   `$...$` if `TRUE`. Defaults to `FALSE`.
#' @param ... Not in use.
#' @return Object of class "dagtex".
#' @export
#'
#' @examples
#' dagtex(
#'   node_options = list(shape = "ellipse"),
#'   swig_options = list(gap = "3pt", line_color_right = "red")
#' ) %>%
#'   add_node(list(c("$A_0$", "$a_0$"), "$L_1$", c("$A_1$", "$a_1$"), "$Y$")) %>%
#'   add_edge(from = "$a_0$", to = c("$L_1$", "$A_1$", "$Y$")) %>%
#'   add_edge(from = "$L_1$", to = c("$A_1$", "$Y$")) %>%
#'   add_edge(from = "$a_1$", to = "$Y$")
add_node <- function(dag, names, options = NULL, x = NULL, y = NULL,
                     right_of = NULL, left_of = NULL,
                     above = NULL, below = NULL,
                     adorn_math = NULL, ...) {
  for (i in seq_along(names)) {

    if (i == 1) {
      args <- c(
        dag = list(dag), name = list(names[[i]]),
        options = list(options),
        x = x, y = y, right_of = right_of, left_of = left_of,
        above = above, below = below, adorn_math = adorn_math,
        is_swig = length(names[[i]]) > 1,
        ...
      )

    } else {
      args <- c(
        dag = list(dag), name = list(names[[i]]),
        options = list(options), adorn_math = adorn_math,
        is_swig = length(names[[i]]) > 1,
        ...
      )
    }

    dag <- do.call(add_single_node, args)
  }

  dag
}

#' Create a split node for a SWIG
#' @param dag Object of class "dagtex", created with [dagtex()].
#' @param left Name to print on the left (or upper) side of the split node.
#' @param right Name to print on the right (or lower) side of the split node.
#' @param options List of swig node options specific to this split node. For details, see
#'   [dagtex()].
#' @param ...  Other named arguments passed to [add_swig()].
#' @return Object of class "dagtex".
#'
#' @export
#'
#' @examples
#' dagtex() %>%
#'   add_swig_node("A", "a")
#' # is equivalent to
#' dagtex() %>%
#'   add_node(c("A", "a"), is_swig = TRUE)
add_swig_node <- function(dag, left, right, options = NULL, ...) {
  add_single_node(dag, name = c(left, right), options = options, is_swig = TRUE, ...)
}

#' Check for SWIG nodes
#' @keywords internal
#' @return Logical indicating whether there are any split nodes in the graph.
any_swig_nodes <- function(dag) {
  any(purrr::map_lgl(dag$nodes, ~ .x$is_swig))
}

#' Add node to the internal DAG structure
#' @keywords internal
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

#' Put position-related options into the correct format
#' @keywords internal
#' @return Tikz code to set location of node (e.g., right = of A)

get_node_position <- function(dag, id, right_of = NULL,
                              left_of = NULL, above = NULL, below = NULL) {
  # don't set position if this is the first node
  if (id == 1) {
    return(NULL)
  }

  positions <- c(right_of %||% NA, left_of %||% NA, above %||% NA, below %||% NA)
  positions_not_na <- purrr::map_lgl(positions, ~ !is.na(.x))

  if (any(positions_not_na)) {
    location <- c("right", "left", "above", "below")[positions_not_na]

    next_to <- positions[positions_not_na] %>%
      unique() %>%
      purrr::map_dbl(~ ifelse(is.character(.x), get_id(dag, .x), .x))

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

#' Get coordinates for adding a new node
#' @keywords internal
#' @return Vector of x,y coordinates at which to place node
get_node_coords <- function(dag, id, coords, right_of = NULL,
                            left_of = NULL, above = NULL, below = NULL) {
  if (is.null(coords) & id == 1) {
    return(c(0, 0))
  }

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

  c((id - 1) * 2, 0)
}

count_nodes <- function(dag) length(dag$nodes)
