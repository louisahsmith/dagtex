#' Plot DAGs
#'
#' @param .dag
#' @param density
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
plot_dagtex <- function(.dag, density = 320, ...) {

  latex_code <- get_latex_code(.dag, add_header = FALSE)

  tikz_opts <- get_tikz_library(.dag)

  pkg_opts <- texPreview::build_usepackage(pkg = 'tikz',
                                           uselibrary = tikz_opts)

  texPreview::tex_preview(
    latex_code,
    usrPackages = pkg_opts,
    density = density,
    fileDir = system.file("tex", package = "dagtex"),
    cleanup = c("aux", "log", "txt", "Doc", "png", "tex"),
    ...)
}

#' Draw DAGs with knitr
#' @importFrom knitr knit_print
#'
#' @param x
#' @param ...
#' @export
knit_print.dagtex <- function(x, density = knitr::opts_current$get("density"),
                              fig.path = knitr::opts_current$get("fig.path"),
                              ...) {

  latex_code <- get_latex_code(x, add_header = FALSE)

  if (knitr::is_latex_output()) return(knitr::asis_output(latex_code))

  fig.path <- fig.path %||% "tikz"
  density <- density %||% 360

  if (!dir.exists(fig.path)) dir.create(fig.path, recursive = TRUE)

  tikz_opts <- get_tikz_library(x)

  pkg_opts <- texPreview::build_usepackage(pkg = 'tikz',
                                           uselibrary = tikz_opts)

  filename <- texPreview::tex_preview(
        latex_code,
        usrPackages = pkg_opts,
        fileDir = fig.path,
        density = density,
        stem = runif(1),
        cleanup = c("aux", "log", "txt", "Doc", "tex"),
        returnType = "engine")

  knitr::include_graphics(filename)
}


#' Explicitly draw DAG
#'
#' @param x
#'
#' @param ...
#'
#' @export
#' @method print dagtex
print.dagtex <- function(x, ...) {
  nodes_and_edges <- x[c("nodes", "edges")]
  is_empty_dag <- all(purrr::map_lgl(nodes_and_edges, purrr::is_empty))

  if (is_empty_dag) {
    cat("An empty DAG")
    return(invisible(x))
  }

  #  wrap in print for print.magick-image when obj is HTML
  print(plot_dagtex(x, ...), info = FALSE)
}

#' @export
#' @method plot dagtex
plot.dagtex <- print.dagtex

#' Insert LaTeX
#'
#' @param .dag
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
insert_latex <- function(.dag, ...) {
  .dag$latex <- c(.dag$latex, ...)

  .dag
}

#' Create tikz picture
#'
#' @param ...
#' @param scale
#' @param scale_x
#' @param scale_y
#'
#' @return
#' @export
#'
#' @examples
# tikz_picture <- function(..., scale = NULL, scale_x = NULL, scale_y = NULL) {
#   # TODO: should just be options?
#   dag_scale <- paste0(
#     "[",
#     ifelse(is.null(scale), "", paste0("scale=", scale)),
#     ifelse(is.null(scale_x), "", paste0("xscale=", scale_x)),
#     ifelse(is.null(scale_y), "", paste0("yscale=", scale_y)),
#     "]"
#   )
#
#   if (dag_scale == "[]") dag_scale <- NULL
#
#   begin_tikzpicture <- paste0("\\begin{tikzpicture}", dag_scale)
#
#
#   paste(
#     c(
#       begin_tikzpicture,
#       ...,
#       "\\end{tikzpicture}"
#     ),
#     collapse = "\n"
#   )
# }

#' Get LaTeX code
#'
#' @param .dag
#' @param add_header
#'
#' @return
#' @export
#'
#' @examples
#'


get_latex_code <- function(.dag, add_header = TRUE) {

  edge_opts <- make_tex_opts(.dag$edge_options)
  to_paste_1 <- paste0("\\begin{tikzpicture}[every path/.style={>=stealth,thick,",
  edge_opts, "}]")

  swig_opts <- make_tex_opts(.dag$swig_options)
  split <- .dag$swig_options$split %||% "v"
  to_paste_2 <- paste0("\\tikzset{swig ", split, "split={",
         swig_opts, "}}")

  node_opts <- make_tex_opts(.dag$node_options)
  draw <- any(c("rectangle", "circle", "ellipse", "circle split",
            "forbidden sign", "diamond", "cross out", "strike out",
            "regular polygon", "star") %in% .dag$node_options)

  to_paste_3 <- paste0("\\tikzstyle{every node}=[solid,text=black,",
                       ifelse(draw, "draw,", ""),
                       node_opts, "]")

  if (add_header) {
    tikz_opts <- get_tikz_library(.dag)
    pkg_opts <- texPreview::build_usepackage(pkg = 'tikz',
                                             uselibrary = tikz_opts)
  } else pkg_opts <- NULL

  latex_code <- paste(c(pkg_opts, to_paste_1, to_paste_2, to_paste_3,
                        latexify_dag(.dag), "\\end{tikzpicture}"),
                        collapse = "\n")

  structure(latex_code,
            class = "latex_code")
}

get_tikz_library <- function(.dag, ...) {
  tikz_opts <- '\\usetikzlibrary{positioning, calc, shapes.geometric,
  shapes.multipart, shapes, arrows.meta, arrows, decorations.markings,
  external, trees, decorations.pathmorphing, positioning'
  tikz_opts <- ifelse(any_swig_nodes(.dag),
                      paste0(tikz_opts, ", shapes.swigs}"),
                      paste0(tikz_opts, "}"))
  tikz_opts
}

print.latex_code <- function(x, ...) {
  cat(x, ...)
}

latexify_dag <- function(.dag) {

  nodes_latex <- purrr::map(.dag$nodes, latexify_node, .dag = .dag)
  edges_latex <- purrr::map_chr(.dag$edges, latexify_edge, .dag = .dag)
  nodes_and_edges <- c(unlist(nodes_latex), edges_latex)

  paste(nodes_and_edges, collapse = "\n")
}


make_tex_opts <- function(opts, exclude = c("split"), remove = c("linetype")) {
  opts <- opts[!names(opts) %in% exclude]
  new_names <- gsub(remove, "", names(opts))
  new_names <- gsub("arrowhead", ">", new_names)
  new_names <- gsub("\\_", " ", new_names)
  if (length(new_names) < 1) new_names <- ""
  args <- purrr::map2_chr(new_names, opts, ~paste(.x, .y, sep = "="))
  args <- gsub("\\,\\s*\\=", ",", paste(args, collapse = ","))
  args <- sub("^\\=", "", args)
  args
}

latexify_node <- function(.node, .dag) {

  node_id <- paste0("(", .node$id, ") ")
  node_text <- paste0("{", .node$name, "}")

  poss_opts <- .node$options

  for_swig <- grepl("(upper)|(lower)|(left)|(right)|(gap)", names(poss_opts))
  node_opts <- poss_opts[!for_swig]
  swig_opts <- poss_opts[for_swig]

  draw <- any(c("rectangle", "circle", "ellipse", "circle split",
            "forbidden sign", "diamond", "cross out", "strike out",
            "regular polygon", "star") %in% node_opts)

  compiled_options <- paste0(ifelse(draw, "draw,", ""),
                                    .node$position, ",",
                             make_tex_opts(node_opts))

  if (.node$is_swig) {
    split <- .node$options$split %||% .dag$swig_options$split
    split <- split %||% "v"
    left <- ifelse(split == "v", "left", "upper")
    right <- ifelse(split == "v", "right", "lower")
    main_part <- paste0(compiled_options,
                      ", shape=swig ", split, "split, swig ",
                      split, "split={", make_tex_opts(swig_opts), "}]{")
    left_part <- paste0("\\nodepart{", left, "}{", .node$name[1], "}")
    right_part <- paste0("\\nodepart{",right, "}{", .node$name[2], "}}")

    return(paste0("\\node[name=",.node$id, ",",
                        main_part, left_part,right_part, ";"))
  } else {

    compiled_options <- paste0(compiled_options, "] ")
    if (compiled_options == "draw, ] ") compiled_options <- "] "

    return(paste0("\\node[", compiled_options, node_id,
                  node_text, " ;"))
  }

}


latexify_edge <- function(.edge, .dag) {

  arrow_type <- ifelse(.edge$is_double_arrow, "<->,",
                       ifelse(.edge$is_headless, "",
                       "->,"))

  edge_options <- paste0("[", arrow_type, make_tex_opts(.edge$options), "]")

  edge_from <- paste0(" (", .edge$from, ") ")
  edge_to <- paste0("(", .edge$to, ")")

  .edge$curve <- .edge$curve %||% "up"

  curve_in <- .edge$curve_in_degree %||%
    ifelse(.edge$is_curved & .edge$curve == "up", 160,
           ifelse(.edge$is_curved, -160, NA))

  curve_out <- .edge$curve_out_degree %||%
    ifelse(.edge$is_curved & .edge$curve == "up", 25,
           ifelse(.edge$is_curved, -25, NA))

  if (!is.na(curve_in) & !is.na(curve_out)) {
    line_code <- "to "
    line_curve <- paste0("[out=", curve_out, ", in=", curve_in, "]")
  } else {
    line_code <- "-- "
    line_curve <- NULL
  }

  paste0(
    "\\draw",
    edge_options,
    edge_from,
    line_code,
    line_curve %||% "",
    .edge$annotate %||% "",
    edge_to,
    ";"
  )
}


