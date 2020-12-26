#' Add counterfactual notation to SWIG
#' @export

adorn_counterfactuals <- function(dag, notation = getOption("dagtex.notation"),
                                  text_color = NULL) {

  notation <- match.arg(notation, c("superscript", "subscript", "parentheses",
                                    "parenthesis", "parens"))

  text_color <- text_color %||% dag$swig_options[["right_text"]] %||% dag$swig_options[["lower_text"]]

  # strip dollar signs
  for (n in seq_along(dag$nodes)) {
    dag$nodes[[n]]$new_name <- purrr::map_chr(dag$nodes[[n]]$name, ~sub("^\\$(.*)\\$$", "\\1", .x))
    dag$nodes[[n]]$had_dollar <- dag$nodes[[n]]$name != dag$nodes[[n]]$new_name
  }

  swig_nodes <- dag$nodes[purrr::map_lgl(dag$nodes, ~.x$is_swig)]
  swig_names <- purrr::map_chr(swig_nodes, ~.x$new_name[2])
  swig_ids <- purrr::map_dbl(swig_nodes, ~.x$id)
  directed_edges <- dag$edges[purrr::map_lgl(dag$edges, ~!.x$is_double_arrow & !.x$is_headless)]
  all_pairs <- purrr::map_dfr(directed_edges, ~data.frame(from = as.numeric(sub("(.*?)\\..*", "\\1", .x$from)),
                                                          to = as.numeric(sub("(.*?)\\..*", "\\1", .x$to))))
  descendants <- list()
  for (swig in swig_ids) {
    if (!swig %in% all_pairs$from) {descendants[[swig]] <- NULL; next}
    direct <- all_pairs[all_pairs$from == swig, 2]
    descendants[[swig]] <- direct
    while (!purrr::is_empty(direct)) {
      next_level <- all_pairs[all_pairs$from %in% direct, 2]
      descendants[[swig]] <- union(descendants[[swig]], next_level)
      direct <- next_level
    }
  }
  names(descendants)[swig_ids] <- swig_names

  for (i in seq_along(descendants)) {
    if (is.null(descendants[[i]])) next
    for (j in descendants[[i]]) {
      dag$nodes[[j]]$counterfactual <- paste(c(dag$nodes[[j]]$counterfactual,
                                              names(descendants[i])), collapse = ",")
    }
  }

  color <- if (!is.null(text_color))
    c(paste0("\\textcolor{", text_color, "}{"), "}") else c("", "")

  for (n in seq_along(dag$nodes)) {
    if (is.null(dag$nodes[[n]]$counterfactual)) next # can't stop, must put back dollars? or do after
    adorn_math <- dag$nodes[[n]]$adorn_math %||% dag$adorn_math
    if (notation == "superscript") {
      surround <- if (adorn_math | dag$nodes[[n]]$had_dollar[1]) c("^{", "}") else
        c("\\textsuperscript{", "}")
    } else if (notation == "subscript") {
      surround <- if (adorn_math | dag$nodes[[n]]$had_dollar[1]) c("_{", "}") else
        c("\\textsubscript{", "}")
    } else surround <- c("(", ")")

    dag$nodes[[n]]$new_name[1] <- paste0(dag$nodes[[n]]$new_name[1],
                                     color[1],  surround[1],
                                      dag$nodes[[n]]$counterfactual,
                                      surround[2], color[2])
  }

  # if there were dollar signs, put back the dollar signs
  for (n in seq_along(dag$nodes)) {
    dag$nodes[[n]]$name <- purrr::map2_chr(dag$nodes[[n]]$new_name, dag$nodes[[n]]$name,
                    ~sub("^(\\$*)(.*?)(\\$*)$",
                         paste0("\\1", gsub("\\\\", "\\\\\\\\", .x), "\\3"),
                         .y))
  }
  dag
}


#' Turn a DAG into a SWIG
#' @export
swigify <- function(dag, intervention_nodes, ...) {
  intervention_ids <- purrr::map_dbl(intervention_nodes, get_id, dag = dag)
  for (n in intervention_ids) {
    dag$nodes[[n]]$is_swig <- TRUE
    dag$nodes[[n]]$name <- c(dag$nodes[[n]]$name, tolower(dag$nodes[[n]]$name))
  }
  dag
}


#' Turn into a complete DAG
#' @param dag
#' @param arrow_type One of "directed", "headless", "double". (Only "directed" really works easily with SWIG nodes, the others take some manual manipulation, at least for now.)
#' @param options A list of edge options for each of the new options. Passed to [add_edge].
#' @param ... Other options passed to [add_edge].
#' @export

dag_complete <- function(dag, arrow_type = "directed", options = NULL, ...) {

  arrow_type <- match.arg(arrow_type, c("directed", "headless", "double"))

  max_id <- count_nodes(dag)

  directed_edges <- dag$edges[purrr::map_lgl(dag$edges, ~!.x$is_double_arrow & !.x$is_headless)]

  if (arrow_type == "headless") directed_edges <- dag$edges[purrr::map_lgl(dag$edges, ~.x$is_headless)]
  if (arrow_type == "double") directed_edges <- dag$edges[purrr::map_lgl(dag$edges, ~.x$is_double_arrow)]

  suppressMessages(
  all_pairs <- purrr::map_dfr(directed_edges, ~data.frame(from = as.numeric(sub("(.*?)\\..*", "\\1", .x$from)),
                                                          to = as.numeric(sub("(.*?)\\..*", "\\1", .x$to))))
  )

  for (i in seq_len(max_id)) {
    existing <- if(purrr::is_empty(all_pairs)) NA else all_pairs$to[all_pairs$from == i]
    dag <- dag %>%
      add_many_edges(from = i, to = setdiff(i:max_id, c(existing, i)),
                      is_headless = arrow_type == "headless",
                      is_double_arrow = arrow_type == "double",
                     is_curved = TRUE, options = options, ...)
  }
  dag

}


