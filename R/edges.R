#' Add edges
#'
#' @param .dag
#' @param .from
#' @param .to
#' @param start_position
#' @param end_position
#' @param .options https://gist.github.com/AndiH/f99d9b0cbd3519c27af5b96cfbeff97c
#' @param is_curved
#' @param curve
#' @param curve_in_degree
#' @param curve_out_degree
#' @param is_double_arrow
#' @param is_headless
#' @param annotate
#' @param ...
#'
#'
#' @return
#' @export
#'
#' @examples
#'
#' @rdname add_edge
add_edge <- function(.dag, .from, .to, start_position = NULL, end_position = NULL,
                     .options = NULL, curve = NULL, is_curved = !is.null(curve),
                     curve_in_degree = NULL, curve_out_degree = NULL,
                     is_double_arrow = FALSE, is_headless = FALSE,
                     annotate = NULL, ...) {

  # default
  if (is.null(curve) & is_curved) curve <- "up"

  id <- count_edges(.dag) + 1

  .from <- get_id(.dag, .from)
  .to <- get_id(.dag, .to)

  if (anyNA(c(.from, .to))) stop("Can't find node to add edge; check names. If multiple nodes have the same name, you must use node number.")

  add_edge_to_dag(
    .dag = .dag,
    .id = id,
    .from = .from,
    .to = .to,
    start_position = start_position,
    end_position = end_position,
    .options = .options,
    is_curved = is_curved,
    curve =  curve,
    curve_in_degree = curve_in_degree,
    curve_out_degree = curve_out_degree,
    is_double_arrow = is_double_arrow,
    is_headless = is_headless,
    annotate = annotate
  )
}

#' @export
#' @rdname add_edge
#' @examples
#' dagtex(.node_options = list(shape = "circle")) %>%
#'  add_node("$A_0$") %>%
#'  add_node("$L_1$") %>%
#'  add_node("$A_1$") %>%
#'  add_node("$Y$") %>%
#'  add_many_edges(.from = "$A_0$", .to = c("$L_1$","$A_1$", "$Y$")) %>%
#'  add_many_edges(.from = "$L_1$", .to = c("$A_1$", "$Y$")) %>%
#'  add_edge(.from = "$A_1$", .to = "$Y$")

add_many_edges <- function(.dag, .from, .to, .options = NULL,
                      is_curved = TRUE, start_curve = "up", ...) {

  to_ids <- purrr::map_dbl(.to, get_id, .dag = .dag)

  from_id <- get_id(.dag, .from)

  if (!is_curved) {
    for (i in seq_along(to_ids)) {
      args <- c(.dag = list(.dag), .from = .from,
                .to = to_ids[i], .options = list(.options), ...)
      .dag <- do.call(add_edge, args)
    }
    return(.dag)
  }

  not_adj <- to_ids[to_ids - from_id > 1]
  adj <- setdiff(to_ids, not_adj)
  next_curve <- ifelse(start_curve == "up", "down", "up")

  for (i in seq_along(adj)) {
    args <- c(.dag = list(.dag), .from = .from,
              .to = adj[i], .options = list(.options), ...)
    .dag <- do.call(add_edge, args)
  }

  for (i in seq_along(not_adj)) {
    curve = ifelse(i %% 2 == 1, start_curve, next_curve)
    args <- c(.dag = list(.dag), .from = .from,
              .to = not_adj[i], curve = curve,
              .options = list(.options), ...)
    .dag <- do.call(add_edge, args)
  }

  .dag
}

#' @export
#' @rdname add_edge
add_curved_edge <- function(.dag, .from, .to, .options = NULL, curve = "up", ...) {
  add_edge(.dag = .dag, .from = .from, .to = .to, curve = curve, .options = .options, is_curved = TRUE, ...)
}

add_edge_to_dag <- function(.dag, .id, .from, .to, start_position = NULL,
                            end_position = NULL, .options = NULL, is_curved = FALSE,
                            curve = "up",
                            curve_in_degree = NULL,
                            curve_out_degree = NULL,
                            is_double_arrow = FALSE,
                            is_headless = FALSE,
                            annotate = NULL) {

  .from <- process_position(.from, start_position)
  .to <- process_position(.to, end_position)


  edge <- structure(
    list(
      id = .id,
      from = .from,
      to = .to,
      is_curved = is_curved,
      curve =  curve,
      curve_in_degree = curve_in_degree,
      curve_out_degree = curve_out_degree,
      options = .options,
      is_double_arrow = is_double_arrow,
      is_headless = is_headless,
      annotate = annotate
    ),
    class = "dagtex_edge"
  )


  .dag$edges[[.id]] <- edge

  .dag
}


#' @export
#' @rdname add_edge
#'

annotate_edge <- function(text, placement = "midway", position = "above",
                          size = "normalsize", color = NULL) {
  size <- match.arg(size, c("tiny", "scriptsize", "footnotesize", "small",
                             "normalsize", "large", "Large", "LARGE", "huge", "Huge"))
  paste0("node[draw=none, text=", color, ", ", placement, ", ", position, "]{\\", size, " ", text, "}")
}

count_edges <- function(.dag) length(.dag$edges)

process_position <- function(target, position) {
  ifelse(
    !is.null(position),
    paste0(target, ".", position),
    target
  )
}

get_id <- function(.dag, .var) {
  if (is.numeric(.var)) return(.var)
   node_names <- unlist(purrr::map(.dag$nodes, "name"))
   swig_nodes <- purrr::map_lgl(.dag$nodes, "is_swig")
   node_ids <- purrr::map_dbl(.dag$nodes, "id")
   node_ids <- unlist(purrr::map2(node_ids, swig_nodes, ~rep(.x, ifelse(.y, 2, 1))))
   node_index <- which(node_names == .var)
   id <- node_ids[node_index]
   ifelse(length(id) != 1, NA_real_, id)
}
