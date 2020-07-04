#' @export

adorn_counterfactuals <- function(.dag, text_color = NULL) {
  text_color <- text_color %||% .dag$swig_options[["right_text"]] %||% .dag$swig_options[["lower_text"]]

  # strip dollar signs
  for (n in seq_along(.dag$nodes)) {
    .dag$nodes[[n]]$name <- purrr::map_chr(.dag$nodes[[n]]$name, ~sub("^\\$(.*)\\$$", "\\1", .x))
  }

  swig_nodes <- .dag$nodes[purrr::map_lgl(.dag$nodes, ~.x$is_swig)]
  swig_names <- purrr::map_chr(swig_nodes, ~.x$name[2])
  swig_ids <- purrr::map_dbl(swig_nodes, ~.x$id)
  directed_edges <- .dag$edges[purrr::map_lgl(.dag$edges, ~!.x$is_double_arrow & !.x$is_headless)]
  all_pairs <- purrr::map_dfr(directed_edges, ~data.frame(from = .x$from,
                                                          to = .x$to))
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
      .dag$nodes[[j]]$counterfactual <- paste(c(.dag$nodes[[j]]$counterfactual,
                                              names(descendants[i])), collapse = ",")
    }
  }

  color1 <- color2 <- NULL
  color1 <- if (!is.null(text_color)) paste0("\\textcolor{", text_color, "}{")
  color2 <- if (!is.null(text_color)) "}"

  for (n in seq_along(.dag$nodes)) {
    if (is.null(.dag$nodes[[n]]$counterfactual)) next
    .dag$nodes[[n]]$name[1] <- paste0(.dag$nodes[[n]]$name[1],
                                      "^{",
                                      color1,
                                      .dag$nodes[[n]]$counterfactual,
                                      color2,
                                      "}")
  }

  .dag$adorn_math <- TRUE

  .dag
}

#' Turn into a complete DAG
#' @param .dag
#' @param arrow_type One of "directed", "headless", "double". (Only directed really works with SWIG nodes, the others take some manual manipulation, at least for now.)
#' @export

complete <- function(.dag, arrow_type = "directed", .options = NULL, ...) {

  max_id <- count_nodes(.dag)

  directed_edges <- .dag$edges[purrr::map_lgl(.dag$edges, ~!.x$is_double_arrow & !.x$is_headless)]

  if (arrow_type == "headless") directed_edges <- .dag$edges[purrr::map_lgl(.dag$edges, ~.x$is_headless)]
  if (arrow_type == "double") directed_edges <- .dag$edges[purrr::map_lgl(.dag$edges, ~.x$is_double_arrow)]

  suppressMessages(
  all_pairs <- purrr::map_dfr(directed_edges, ~data.frame(from = .x$from,
                                                          to = .x$to))
  )

  for (i in seq_len(max_id)) {
    existing <- ifelse(purrr::is_empty(all_pairs), NA, all_pairs$to[all_pairs$from == i])
    .dag <- .dag %>%
      add_many_edges(.from = i, .to = setdiff(i:max_id, c(existing, i)),
                      is_headless = arrow_type == "headless",
                      is_double_arrow = arrow_type == "double",
                     is_curved = TRUE, .options = .options, ...)
  }
  .dag

}


