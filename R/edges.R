#' Add a single edge between two nodes in a DAG
#'
#' Connect two nodes using a straight or curved line. By default, edges are
#' directed (`from` to `to`) but undirected or bidirectional arrows are also
#' possible. The angles at which the edge leaves and enters the nodes, and the
#' location at which it does so, can be finely tuned. [add_curved_edge()] is a
#' shortcut for a (hopefully) reasonably curved edge.
#'
#' @param dag Object created by [dagtex()].
#' @param from,to The names (or numeric id) of nodes create by [add_node()] or
#'   related functions.
#' @param start_position,end_position The start and end positions of the edge in
#'   any manner acceptable to Tikz (e.g., a coordinate in degrees like 30 or a
#'   direction like "south west")
#' @param options A list of edge options specific to this edge; see [dagtex()].
#' @param is_curved Logical. Whether the edge should be curved.
#' @param curve Direction of the curve, if any: "up" or "down".
#' @param curve_in_degree,curve_out_degree Angle in degrees at which the edge
#'   leaves and enters the nodes.
#' @param is_double_arrow,is_headless Logical. Default is directed arrow; these
#'   options can be used to specify headless or two-headed arrows.
#' @param annotate Annotation specified with [annotate_edge()].
#' @param ... Not currently used.
#'
#'
#' @return The same object of class "dagtex" with an additional edge.
#' @keywords internal
#' @examples
#' dagtex(edge_options = list(line_type = "dotted", color = "red")) %>%
#'   add_node(c("A", "B", "C")) %>%
#'   add_edge("A", "C", curve = "down", is_double_arrow = TRUE,
#'            annotate = annotate_edge("example", position = "below"),
#'            options = list(color = "teal", line_type = "solid")) %>%
#'   add_edge(1, 2, start_position = "north", curve_in_degree = 0, curve_out_degree = 90)

add_single_edge <- function(dag, from, to, start_position = NULL, end_position = NULL,
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
    curve = curve,
    curve_in_degree = curve_in_degree,
    curve_out_degree = curve_out_degree,
    is_double_arrow = is_double_arrow,
    is_headless = is_headless,
    annotate = annotate
  )
}

#' Add edge(s) to a DAG
#'
#' Connect nodes using a straight or curved line. By default, edges are
#' directed (`from` to `to`) but undirected or bidirectional arrows are also
#' possible. The angles at which the edge leaves and enters the nodes, and the
#' location at which it does so, can be finely tuned. Vectors of nodes can be
#' used to draw and edge from every node in `from` to a single `to` node,
#' or an edge from a single `from` node to every node in `to`. Edges will be
#' straight between adjacent nodes and curved otherwise if `auto_curve` is specified,
#' but may look funny if nodes are not in a line.
#'
#' @param dag Object created by [dagtex()].
#' @param from,to The names (or numeric id) of nodes create by [add_node()] or
#'   related functions. Only one of either `from` or `to` can be a vector.
#' @param start_position,end_position The start and end positions of the edges in
#'   any manner acceptable to Tikz (e.g., a coordinate in degrees like 30 or a
#'   direction like "south west"). If  multiple `from` or `to` nodes are given,
#'   matching vectors of `start_position` and `end_position` can be given.
#'   Otherwise dagtex() will attempt to place the edges appropriately.
#' @param options A list of edge options specific to these edges; see [dagtex()].
#' @param curve Direction of the curve, if any: "up" or "down". Can be a vector
#'   if  multiple `from` or `to` nodes are given, otherwise applies to all edges.
#' @param auto_curve One of "up", "down", or FALSE (the default). If "up" or "down",
#'   dagtex() will attempt to curve the edges appropriately, with the first
#'   curved edge being either "up" or "down". If `auto_curve` is not FALSE,
#'   `curve` will be ignored if multiple `from` or `to` nodes are given.
#' @param curve_in_degree,curve_out_degree Angle in degrees at which the edge
#'   leaves and enters the nodes. If  multiple `from` or `to` nodes are given,
#'   matching vectors of `curve_in_degree` and `curve_out_degree` can be given,
#'   otherwise applies to all edges.
#' @param is_double_arrow,is_headless Logical. Default is directed arrow; these
#'   options can be used to specify headless or two-headed arrows. Can be a vector
#'   if  multiple `from` or `to` nodes are given, otherwise applies to all edges.
#' @param annotate Annotation specified with [annotate_edge()]. Can be a vector
#'   if  multiple `from` or `to` nodes are given, otherwise applies to all edges.
#' @param ... Not currently used.
#'
#' @export
#' @examples
#' dagtex(node_options = list(shape = "circle")) %>%
#'   add_node("$A_0$") %>%
#'   add_node("$L_1$") %>%
#'   add_node("$A_1$") %>%
#'   add_node("$Y$") %>%
#'   add_edge(from = "$A_0$", to = c("$L_1$", "$A_1$", "$Y$")) %>%
#'   add_edge(from = "$L_1$", to = c("$A_1$", "$Y$")) %>%
#'   add_edge(from = "$A_1$", to = "$Y$")
#'
#' dagtex() %>%
#'   add_node(c("A", "B", "C", "D")) %>%
#'   add_edge(from = c("A", "B", "C"), to = "D")
add_edge <- function(dag, from, to, start_position = NULL, end_position = NULL,
                     options = NULL, curve = NULL, auto_curve = FALSE,
                     curve_in_degree = NULL, curve_out_degree = NULL,
                     is_double_arrow = FALSE, is_headless = FALSE,
                     annotate = NULL, ...) {
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

  curve <- expand_args(curve, length(to_ids))
  curve_out_degree <- expand_args(curve_out_degree, length(to_ids))
  curve_in_degree <- expand_args(curve_in_degree, length(to_ids))
  is_double_arrow <- expand_args(is_double_arrow, length(to_ids))
  is_headless <- expand_args(is_headless, length(to_ids))
  annotate <- expand_args(annotate, length(to_ids))

  if (isFALSE(auto_curve) | is.null(auto_curve) | length(to_ids) == 1) {
    for (i in seq_along(to_ids)) {
      if (backwards) {
        args <- c(
          dag = list(dag), to = from,
          from = to_ids[i], options = list(options),
          curve = curve[i],
          # these are backwards because arguments have been switched
          curve_in_degree = curve_out_degree[i], curve_out_degree = curve_in_degree[i],
          is_double_arrow = is_double_arrow[i], is_headless = is_headless[i],
          annotate = annotate[i], ...
        )
      } else {
        args <- c(
          dag = list(dag), from = from,
          to = to_ids[i], options = list(options), curve = curve[i],
          curve_in_degree = curve_in_degree[i], curve_out_degree = curve_out_degree[i],
          is_double_arrow = is_double_arrow[i], is_headless = is_headless[i],
          annotate = annotate[i],
          ...
        )
      }
      dag <- do.call(add_single_edge, args)
    }
    return(dag)
  }

  not_adj <- to_ids[abs(to_ids - from_id) > 1]
  adj <- setdiff(to_ids, not_adj)
  next_curve <- ifelse(auto_curve == "up", "down", "up")

  for (i in seq_along(adj)) {
    if (backwards) {
      args <- c(
        dag = list(dag), to = from,
        from = adj[i], options = list(options),
        # these are backwards because arguments have been switched
        curve_in_degree = curve_out_degree[i], curve_out_degree = curve_in_degree[i],
        is_double_arrow = is_double_arrow[i], is_headless = is_headless[i],
        annotate = annotate[i], ...
      )
    } else {
      args <- c(
        dag = list(dag), from = from,
        to = adj[i], options = list(options), curve_in_degree = curve_in_degree[i], curve_out_degree = curve_out_degree[i],
        is_double_arrow = is_double_arrow[i], is_headless = is_headless[i],
        annotate = annotate[i],
        ...
      )
    }
    dag <- do.call(add_single_edge, args)
  }

  for (i in seq_along(not_adj)) {
    curve <- ifelse(i %% 2 == 1, auto_curve, next_curve)
    if (backwards) {
      args <- c(
        dag = list(dag), to = from,
        from = not_adj[i], curve = curve,
        options = list(options),
        # these are backwards because arguments have been switched
        curve_in_degree = curve_out_degree[i], curve_out_degree = curve_in_degree[i],
        is_double_arrow = is_double_arrow[i], is_headless = is_headless[i],
        annotate = annotate[i], ...
      )
    } else {
      args <- c(
        dag = list(dag), from = from,
        to = not_adj[i], curve = curve,
        options = list(options),
        curve_in_degree = curve_in_degree[i], curve_out_degree = curve_out_degree[i],
        is_double_arrow = is_double_arrow[i], is_headless = is_headless[i],
        annotate = annotate[i], ...
      )
    }
    dag <- do.call(add_single_edge, args)
  }

  dag
}

#' @keywords internal
expand_args <- function(arg, len) {
  if (is.null(arg)) return(NULL)
  if (length(arg) < len) {
    if (length(arg) != 1) {
      warning(paste0("Using only the first element of ", deparse(substitute(arg))))
    }
    arg <- rep(arg, length.out = len)
  }
  arg
}

#' @rdname add_edge
#' @examples
#' dagtex(edge_options = list(line_type = "dotted", color = "red")) %>%
#'   add_node(c("A", "B", "C")) %>%
#'   add_curved_edge("A", "C", curve = "down", is_double_arrow = TRUE,
#'            annotate = annotate_edge("example", position = "below"),
#'            options = list(color = "teal", line_type = "solid")) %>%
#'   add_curved_edge(1, 2, curve = "down")
add_curved_edge <- function(dag, from, to, options = NULL, curve = "up", ...) {
  add_edge(dag = dag, from = from, to = to, curve = curve, options = options, ...)
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
      curve = curve,
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

#' Add text to an edge
#'
#' Annotate the edge between two nodes with some additional text.
#'
#' @param text Text to add to the DAG.
#'
#' @param placement Character, describes where along the edge to place the
#'   annotation. Defaults to "midway", other options include "near start", "at
#'   end", etc. (see TikZ documentation).
#' @param position "above", "below", or "" for positioning text on top of the
#'   edge.
#' @param size Font size, in LaTex-speak. Defaults to "normalsize", other options are
#'   "footnotesize", "LARGE", etc.
#' @param color Text color of annotation.
#'
#' @export
#' @examples
#' dagtex() %>%
#' add_node(c("A", "B")) %>%
#' add_edge("A", "B", annotate = annotate_edge("example"))
#'
#' dagtex() %>%
#' add_node(c("A", "B")) %>%
#' add_edge("A", "B",
#' annotate = annotate_edge("example", color = "teal", size = "tiny",
#' position = "very near start", placement = ""))
#'
annotate_edge <- function(text, placement = "midway", position = "above",
                          size = "normalsize", color = NULL) {
  size <- match.arg(size, c(
    "tiny", "scriptsize", "footnotesize", "small",
    "normalsize", "large", "Large", "LARGE", "huge", "Huge"
  ))
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
  if (is.numeric(.var)) {
    return(.var)
  }
  node_names <- unlist(purrr::map(dag$nodes, "name"))
  swig_nodes <- purrr::map_lgl(dag$nodes, "is_swig")
  node_ids <- purrr::map_dbl(dag$nodes, "id")
  node_ids <- unlist(purrr::map2(node_ids, swig_nodes, ~ rep(.x, ifelse(.y, 2, 1))))
  node_index <- which(node_names == .var)
  id <- node_ids[node_index]
  ifelse(length(id) != 1, NA_real_, id)
}


#' Get numeric IDs for nodes
#'
#' @param dag Object created by [dagtex()].
#'
#' @return A dataframe with columns "name" and "node_id" containing the names
#'   and numeric IDs of the nodes, to be used to reference those nodes
#' @export
#'
#' @examples
#' dagtex() %>%
#'   add_swig_node("X", "x") %>%
#'   add_node("Y") %>%
#'   get_node_ids()

get_node_ids <- function(dag) {
  node_names <- unlist(purrr::map(dag$nodes, "name"))
  swig_nodes <- purrr::map_lgl(dag$nodes, "is_swig")
  node_ids <- purrr::map_dbl(dag$nodes, "id")
  node_ids <- unlist(purrr::map2(node_ids, swig_nodes, ~ rep(.x, ifelse(.y, 2, 1))))
  node_ids <- data.frame("name" = node_names,
    "node_id" = as.numeric(node_ids))
  node_ids
}

