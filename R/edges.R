#' Add a single edge between two nodes in a DAG
#'
#' @param dag Object created by [dagtex()].
#' @param from,to The names (or numeric id) of nodes create by [add_node()] or related functions.
#' @param start_position,end_position The start and end positions of the edge in any manner acceptable to Tikz (e.g., a coordinate in degrees like 30 or a direction like "south west")
#' @param options A list of edge options specific to this edge; see [dagtex()].
#' @param is_curved Logical. Whether the edge should be curved.
#' @param curve Direction of the curve, if any: "up" or "down".
#' @param curve_in_degree,curve_out_degree Angle in degrees at which the edge leaves and enters the nodes.
#' @param is_double_arrow,is_headless Logical. Default is directed arrow; these options can be used to specify headless or two-headed arrows.
#' @param annotate Annotation specified with [annotate_edge()].
#' @param ... Not currently used.
#'
#'
#' @return
#' @export
#'
#'
add_edge <- function(dag, from, to, start_position = NULL, end_position = NULL,
                     options = NULL, is_curved = !is.null(curve), curve = NULL,
                     curve_in_degree = NULL, curve_out_degree = NULL,
                     is_double_arrow = FALSE, is_headless = FALSE,
                     annotate = NULL, ...) {

  # default
  if (is.null(curve) & is_curved) curve <- "up"

  id <- count_edges(dag) + 1

  from <- get_id(dag, from)
  to <- get_id(dag, to)

  if (anyNA(c(from, to))) stop("Can't find node to add edge; check names. If multiple nodes have the same name, you must use node number.")

  add_edge_to_dag(
    dag = dag,
    id = id,
    from = from,
    to = to,
    start_position = start_position,
    end_position = end_position,
    options = options,
    is_curved = is_curved,
    curve =  curve,
    curve_in_degree = curve_in_degree,
    curve_out_degree = curve_out_degree,
    is_double_arrow = is_double_arrow,
    is_headless = is_headless,
    annotate = annotate
  )
}

#' Add several edges at once to a DAG
#'
#' @inheritParams add_edge
#'
#' @export
#' @examples
#' dagtex(node_options = list(shape = "circle")) %>%
#'  add_node("$A_0$") %>%
#'  add_node("$L_1$") %>%
#'  add_node("$A_1$") %>%
#'  add_node("$Y$") %>%
#'  add_many_edges(from = "$A_0$", to = c("$L_1$","$A_1$", "$Y$")) %>%
#'  add_many_edges(from = "$L_1$", to = c("$A_1$", "$Y$")) %>%
#'  add_edge(from = "$A_1$", to = "$Y$")
#'
#' dagtex() %>%
#'  add_many_nodes(c("A", "B", "C", "D")) %>%
#'  add_many_edges(from = c("A", "B", "C"), to = "D")
add_many_edges <- function(dag, from, to, options = NULL,
                      is_curved = TRUE, start_curve = "up", ...) {

  if (length(from) > 1 & length(to) > 1) stop("Can only choose a single \"from\" or \"to\" node at a time")

  # originally written from one to many, backwards allows for many to one
  start_node <- from
  end_node <- to
  backwards <- FALSE

  if (length(start_node) > length(end_node)) {
    from <- end_node
    to <- start_node
    backwards <- TRUE
  }

  to_ids <- purrr::map_dbl(to, get_id, dag = dag)
  from_id <- get_id(dag, from)

  if (!is_curved) {
    for (i in seq_along(to_ids)) {
      if (backwards) {
        args <- c(dag = list(dag), to = from,
                  from = to_ids[i], options = list(options), ...)
      } else {
        args <- c(dag = list(dag), from = from,
                  to = to_ids[i], options = list(options), ...)
      }
      dag <- do.call(add_edge, args)
    }
    return(dag)
  }

  not_adj <- to_ids[abs(to_ids - from_id) > 1]
  adj <- setdiff(to_ids, not_adj)
  next_curve <- ifelse(start_curve == "up", "down", "up")

  for (i in seq_along(adj)) {
    if (backwards) {
      args <- c(dag = list(dag), to = from,
                from = adj[i], options = list(options), ...)
    } else {
      args <- c(dag = list(dag), from = from,
                to = adj[i], options = list(options), ...)
    }
    dag <- do.call(add_edge, args)
  }

  for (i in seq_along(not_adj)) {
    curve = ifelse(i %% 2 == 1, start_curve, next_curve)
    if (backwards) {
      args <- c(dag = list(dag), to = from,
                from = not_adj[i], curve = curve,
                options = list(options))
    } else {
      args <- c(dag = list(dag), from = from,
                to = not_adj[i], curve = curve,
                options = list(options))
    }
    dag <- do.call(add_edge, args)
  }

  dag
}

#' @param dag
#'
#' @inheritParams add_edge
#'
#' @export
#' @rdname add_edge
add_curved_edge <- function(dag, from, to, options = NULL, curve = "up", ...) {
  add_edge(dag = dag, from = from, to = to, curve = curve, options = options, is_curved = TRUE, ...)
}

#' @keywords internal
add_edge_to_dag <- function(dag, id, from, to, start_position = NULL,
                            end_position = NULL, options = NULL, is_curved = FALSE,
                            curve = "up",
                            curve_in_degree = NULL,
                            curve_out_degree = NULL,
                            is_double_arrow = FALSE,
                            is_headless = FALSE,
                            annotate = NULL) {

  from <- process_position(from, start_position)
  to <- process_position(to, end_position)

  edge <- structure(
    list(
      id = id,
      from = from,
      to = to,
      is_curved = is_curved,
      curve =  curve,
      curve_in_degree = curve_in_degree,
      curve_out_degree = curve_out_degree,
      options = options,
      is_double_arrow = is_double_arrow,
      is_headless = is_headless,
      annotate = annotate
    ),
    class = "dagtex_edge"
  )

  dag$edges[[id]] <- edge

  dag
}

#' Add text to an edge.
#'
#'
#' @param text
#'
#' @param placement
#' @param position
#' @param size
#' @param color
#'
#' @export
#'
#'
annotate_edge <- function(text, placement = "midway", position = "above",
                          size = "normalsize", color = NULL) {
  size <- match.arg(size, c("tiny", "scriptsize", "footnotesize", "small",
                             "normalsize", "large", "Large", "LARGE", "huge", "Huge"))
  paste0("node[draw=none, text=", color, ", ", placement, ", ", position, "]{\\", size, " ", text, "}")
}

#' @keywords internal
count_edges <- function(dag) length(dag$edges)

#' @keywords internal
process_position <- function(target, position) {
  ifelse(
    !is.null(position),
    paste0(target, ".", position),
    target
  )
}

#' @keywords internal
get_id <- function(dag, .var) {
  if (is.numeric(.var)) return(.var)
   node_names <- unlist(purrr::map(dag$nodes, "name"))
   swig_nodes <- purrr::map_lgl(dag$nodes, "is_swig")
   node_ids <- purrr::map_dbl(dag$nodes, "id")
   node_ids <- unlist(purrr::map2(node_ids, swig_nodes, ~rep(.x, ifelse(.y, 2, 1))))
   node_index <- which(node_names == .var)
   id <- node_ids[node_index]
   ifelse(length(id) != 1, NA_real_, id)
}
