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

  if (knitr::is_latex_output()) return(knitr::asis_output(latex_code))

  is_knit_image <- isTRUE(getOption("knitr.in.progress"))

  tikz_opts <- '\\usetikzlibrary{positioning, calc, shapes.geometric,
  shapes.multipart, shapes, arrows.meta, arrows, decorations.markings,
  external, trees, decorations.pathmorphing, positioning, shapes.swigs}'

  pkg_opts <- texPreview::build_usepackage(pkg = c('pgf', 'tikz'),
                                           uselibrary = tikz_opts)


  if (is_knit_image) {
    return(
      texPreview::tex_preview(
        latex_code,
        usrPackages = pkg_opts,
        fileDir = system.file("tex", package = "dagtex"),
        cleanup = c("aux", "log", "txt", "Doc", "png", "tex"),
        density = density,
        resizebox = FALSE,
        returnType = "html",
        ...)
    )
  }

  texPreview::tex_preview(
    latex_code,
    usrPackages = pkg_opts,
    density = density,
    fileDir = system.file("tex", package = "dagtex"),
    cleanup = c("aux", "log", "txt", "Doc", "png", "tex"),
    ...)
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
tikz_picture <- function(..., scale = NULL, scale_x = NULL, scale_y = NULL) {
  # TODO: should just be options?
  dag_scale <- paste0(
    "[",
    ifelse(is.null(scale), "", paste0("scale=", scale)),
    ifelse(is.null(scale_x), "", paste0("xscale=", scale_x)),
    ifelse(is.null(scale_y), "", paste0("yscale=", scale_y)),
    "]"
  )

  if (dag_scale == "[]") dag_scale <- NULL

  begin_tikzpicture <- paste0("\\begin{tikzpicture}", dag_scale)


  paste(
    c(
      begin_tikzpicture,
      "\\tikzset{>=stealth}",
      "\\tikzstyle{Arrow} = [->, thick, preaction = {decorate}]",
      "\\tikzstyle{DoubleArrow} = [<->, thick, preaction = {decorate}]",
      ...,
      "\\end{tikzpicture}"
    ),
    collapse = "\n"
  )
}

#' Get LaTeX code
#'
#' @param .dag
#' @param add_header
#'
#' @return
#' @export
#'
#' @examples
get_latex_code <- function(.dag, add_header = TRUE) {

  latex_code <- tikz_picture(latexify_dag(.dag))

  if (add_header & !(any_swig_nodes(.dag))) {
    tikz_opts <- '\\usetikzlibrary{positioning, calc, shapes.geometric,
    shapes.multipart, shapes, arrows.meta, arrows, decorations.markings,
    external, trees}'

    pkg_opts <- texPreview::build_usepackage(pkg = 'tikz', uselibrary = tikz_opts)
    latex_code <- paste(c(pkg_opts, latex_code), collapse = "\n")
  }

  if (any_swig_nodes(.dag)) {
    if (add_header) {
    tikz_opts <- '\\usetikzlibrary{positioning, calc, shapes.geometric,
    shapes.multipart, shapes, arrows.meta, arrows, decorations.markings,
    external, trees, decorations.pathmorphing, positioning, shapes.swigs}'

    pkg_opts <- texPreview::build_usepackage(pkg = c('pgf', 'tikz'),
                                             uselibrary = tikz_opts)
  } else pkg_opts <- NULL
    if (!is.null(.dag$swig_options)) {
      swig_opts <- make_tex_opts(.dag$swig_options)
      swig_opts <- paste0( "\\\\begin{tikzpicture}\\\n\\\\tikzset{swig vsplit={", swig_opts, "}}")
      latex_code <- sub("\\\\begin\\{tikzpicture\\}", swig_opts, latex_code)
    }
  latex_code <- paste(c(pkg_opts, latex_code), collapse = "\n")
  }

  structure(latex_code, class = "latex_code")
}

print.latex_code <- function(x, ...) {
  cat(x, ...)
}

latexify_dag <- function(.dag) {
  # if (any_swig_nodes(.dag)) {}
  nodes_latex <- purrr::map(.dag$nodes, latexify_node, .dag = .dag)
  edges_latex <- purrr::map_chr(.dag$edges, latexify_edge, .dag = .dag)
  nodes_and_edges <- c(unlist(nodes_latex), edges_latex)

  paste(nodes_and_edges, collapse = "\n")
}

latexify_node <- function(.node, .dag) {
  if (.node$is_swig) return(latexify_swig(.node, .dag = .dag))

  node_options <- compile_node_options(.node, .dag = .dag)
  node_id <- paste0("(", .node$id, ") ")
  node_text <- paste0("{", .node$name, "}")


  paste0(
    "\\node",
    node_options,
    node_id,
    node_text,
    ";"
  )
}

make_tex_opts <- function(opts) {
  new_names <- gsub("\\_", " ", names(opts))
  new_names <- gsub("linetype", "", new_names)
  if (length(new_names) < 1) new_names <- ""
  args <- purrr::map2_chr(new_names, opts, ~paste(.x, .y, sep = "="))
  args <- gsub("\\,\\s*\\=", ",", paste(args, collapse = ","))
  args <- sub("^\\=", "", args)
}

compile_node_options <- function(.node, .dag) {

  all_opts <- .dag$node_options

  poss_opts <- .node$options

  draw <- any(c("rectangle", "circle", "ellipse", "circle split",
            "forbidden sign", "diamond", "cross out", "strike out",
            "regular polygon", "star") %in% c(all_opts, poss_opts))

  compiled_options <- paste0(ifelse(draw, "draw,", ""),
                                    .node$position, ",",
                             make_tex_opts(all_opts), ", ",
                             make_tex_opts(poss_opts))

  node_options <- paste0("[", compiled_options, "] ")
  if (node_options == "[draw, ] ") node_options <- NULL
  node_options
}

latexify_swig <- function(.node, .dag) {

  poss_opts <- .node$options
  main_part <- paste0("\\node[name=",.node$id, ",", .node$position,
                      ", shape=swig vsplit",
                      make_tex_opts(poss_opts), "]{")
  left_part <- paste0("\\nodepart{left}{", .node$name[1], "}")
  right_part <- paste0("\\nodepart{right}{", .node$name[2], "}}")

  paste(
    main_part,
    left_part,
    right_part,
    ";"
  )
}

latexify_edge <- function(.edge, .dag) {

  edge_from <- paste0(" (", .edge$from, ") ")
  edge_to <- paste0("(", .edge$to, ")")
  edge_options <- compile_edge_options(.edge, .dag)

  curve_in <- ifelse(!is.null(.edge$curve_in_degree),
                     .edge$curve_in_degree,
                     ifelse(.edge$is_curved & .edge$curve == "up", 160,
                            ifelse(.edge$is_curved, -160, NA)))

  curve_out <- ifelse(!is.null(.edge$curve_out_degree),
                     .edge$curve_out_degree,
                     ifelse(.edge$is_curved & .edge$curve == "up", 25,
                            ifelse(.edge$is_curved, -25, NA)))

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

compile_edge_options <- function(.edge, .dag) {

  arrow_type <- ifelse(.edge$is_double_arrow, "DoubleArrow", "Arrow")

  all_opts <- .dag$edge_options

  poss_opts <- .edge$options

  compiled_options <- paste0(arrow_type, ",",
                             make_tex_opts(all_opts), ", ",
                             make_tex_opts(poss_opts))

  edge_options <- paste0("[", compiled_options, "]")
  edge_options
}
