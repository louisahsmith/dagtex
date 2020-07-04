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
plot_dagtex <- function(.dag, density = getOption("dagtex.density"), ...) {

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
  density <- density %||% getOption("dagtex.density")

  if (!dir.exists(fig.path)) dir.create(fig.path, recursive = TRUE)

  tikz_opts <- get_tikz_library(x)

  pkg_opts <- texPreview::build_usepackage(pkg = 'tikz',
                                           uselibrary = tikz_opts)

  filename <- texPreview::tex_preview(
    latex_code,
    usrPackages = pkg_opts,
    fileDir = fig.path,
    density = density,
    stem = substr(tempfile(pattern = "dagtex_", tmpdir = ""), 2, 21),
    cleanup = getOption("dagtex.cleanup"),
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

  plot_dagtex(x, ...)
}

#' @export
#' @method plot dagtex
plot.dagtex <- print.dagtex


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

  swig_paste <- pkg_opts <- lines_paste <- angle_paste <- NULL
  swig_paste_l <- swig_paste_r <- swig_paste_lo <- swig_paste_up <- NULL

  edge_opts <- make_tex_opts(.dag$edge_options)
  edge_paste <- paste0("\\begin{tikzpicture}[every path/.style={>=stealth, thick,",
                       edge_opts, "}]")

  draw_node <- "shape" %in% names(.dag$node_options)

  if (any_swig_nodes(.dag)) {

    draw_swig <- "shape" %in% names(.dag$swig_options)

    swig_opts <- split_swig_opts(.dag$swig_options, exclude = "text")

    swig_paste <- paste0("\\tikzset{swig vsplit={gap=3pt,", swig_opts$left_included,
                         ",", swig_opts$right_included, ",", swig_opts$all_included,
                         "}}\n\\tikzset{swig hsplit={gap=3pt,",swig_opts$upper_included,
                         ",", swig_opts$lower_included, ",", swig_opts$all_included,"}}")

    swig_paste_style <- paste0("\tikzset{swig vsplit/.style={opacity = ",
                               as.numeric(draw_swig), ",", swig_opts$all_excluded,
                               ", text opacity = 1}}\n",
                               "\tikzset{swig hsplit/.style={opacity = ",
                               as.numeric(draw_swig), ",", swig_opts$all_excluded,
                               ", text opacity = 1}}")

    # if color is in here, it is TEXT color
    swig_paste_l <-  paste0("\\tikzset{l/.style={", swig_opts$left_excluded, "}}")
    swig_paste_r <-  paste0("\\tikzset{r/.style={", swig_opts$right_excluded, "}}")
    swig_paste_lo <-  paste0("\\tikzset{lo/.style={", swig_opts$lower_excluded, "}}")
    swig_paste_up <-  paste0("\\tikzset{up/.style={", swig_opts$lower_excluded, "}}")

  }

  node_opts <- make_tex_opts(.dag$node_options)

  # start off with defaults so not overwritten by edge style (which applies to all paths)
  node_paste <- paste0("\\tikzstyle{every node}=[solid,black,text=black,",
                       ifelse(draw_node, "draw,", ""),
                       node_opts, "]")

  if (add_header) {
    tikz_opts <- get_tikz_library(.dag)
    pkg_opts <- texPreview::build_usepackage(pkg = 'tikz',
                                             uselibrary = tikz_opts)
  }

  if (sum(.dag$help_lines)) {

    if (!is.numeric(.dag$help_lines) | length(.dag$help_lines) != 2) {
      .dag$help_lines <- get_dimensions(.dag)
    }
    lines_paste <- paste0("\\draw[help lines] (0,0) grid (",
                          .dag$help_lines[1],",",.dag$help_lines[2],");")
  }

  if (sum(.dag$help_angles)) {

    if (!is.numeric(.dag$help_angles)) {
      .dag$help_angles <- seq(0, 360, 30)
    }
    angle_vals <- paste(.dag$help_angles, collapse = ", ")
    node_names <- paste(purrr::map_dbl(.dag$nodes, ~.$id), collapse = ", ")

    angle_paste <- paste(c(paste0("\\foreach \\name in {", node_names,"}"),
                           paste0("\\foreach \\angle in {", angle_vals, "}"),
                           "\\draw[gray, thin, solid] (\\name) -- +(\\angle:1);"),
                         collapse = "\n")
  }

  latex_code <- paste(c(pkg_opts, edge_paste, swig_paste,
                        node_paste, swig_paste_l, swig_paste_r,
                        swig_paste_lo, swig_paste_up, lines_paste,
                        latexify_dag(.dag), angle_paste, "\\end{tikzpicture}"),
                      collapse = "\n")

  structure(latex_code,
            class = "latex_code")
}


get_dimensions <- function(.dag, ...) {

  x <- max(purrr::map_dbl(.dag$nodes, ~.$coords[1]))
  y <- max(purrr::map_dbl(.dag$nodes, ~.$coords[2]))

  c(x, y)
}

#' @export
#'
get_tikz_library <- function(.dag = NULL, has_swig = FALSE, ...) {

  if (!is.null(.dag) & any_swig_nodes(.dag)) has_swig <- TRUE
  tikz_opts <- '\\usetikzlibrary{positioning, calc, shapes.geometric,
  shapes.multipart, shapes, arrows.meta, arrows, decorations.markings,
  external, trees, decorations.pathmorphing, positioning'
  tikz_opts <- ifelse(has_swig,
                      paste0(tikz_opts, ", shapes.swigs}"),
                      paste0(tikz_opts, "}"))
  class(tikz_opts) <- "latex_code"
  tikz_opts
}

#' @export
print.latex_code <- function(x, ...) {
  # clean things up
  x <- gsub("(\\s*\\,\\s*)+", ", ", x)
  x <- gsub("\\,\\s*(\\}|\\])", "\\1", x)
  x <- gsub("(\\{|\\[)\\s*\\,\\s*", "\\1", x)
  cat(x, ...)
}

latexify_dag <- function(.dag) {

  nodes_latex <- purrr::map(.dag$nodes, latexify_node, .dag = .dag)
  edges_latex <- purrr::map_chr(.dag$edges, latexify_edge, .dag = .dag)
  nodes_and_edges <- c(unlist(nodes_latex), edges_latex)

  paste(nodes_and_edges, collapse = "\n")
}

#' @param exclude Options to exclude (character vector)
#' @param remove Names (regular expression) of options to remove (i.e., paste bare argument)
#'
make_tex_opts <- function(opts, exclude = c("split"), remove = c("line_type")) {
  opts <- opts[!names(opts) %in% exclude]
  new_names <- gsub(remove, "", names(opts))
  new_names <- gsub("arrowhead", ">", new_names)
  new_names <- gsub("\\_", " ", new_names) %0% ""
  args <- purrr::map2_chr(new_names, opts, ~paste(.x, .y, sep = "="))
  args <- gsub("\\,\\s*\\=", ",", paste(args, collapse = ","))
  args <- sub("^\\=", "", args)
  args
}

#' @param exclude Text (regular expression) for searching out SWIG options
#' that use the terms left/right/upper/lower but can't be passed directly
#' to the
#'
split_swig_opts <- function(.swig_options, exclude = "text", ...) {
  # excluded are those that do not get placed within the swig vsplit={}
  # list but instead in the /.style={} list
  upper <- grepl("upper", names(.swig_options))
  lower <- grepl("lower", names(.swig_options))
  left <- grepl("left", names(.swig_options))
  right <- grepl("right", names(.swig_options))
  excluded <- grepl(exclude, names(.swig_options))
  upper_excluded <- .swig_options[as.logical(upper * excluded)]
  lower_excluded <- .swig_options[as.logical(lower * excluded)]
  left_excluded <- .swig_options[as.logical(left * excluded)]
  right_excluded <- .swig_options[as.logical(right * excluded)]
  upper_included <- .swig_options[as.logical(upper * !excluded)]
  lower_included <- .swig_options[as.logical(lower * !excluded)]
  left_included <- .swig_options[as.logical(left * !excluded)]
  right_included <- .swig_options[as.logical(right * !excluded)]
  all_included <- .swig_options[["gap"]]
  if (!is.null(all_included)) all_included <- list(gap = all_included)
  all_excluded <- .swig_options[
    !(.swig_options %in% c(upper_excluded, lower_excluded, left_excluded,
                           right_excluded, upper_included, lower_included,
                           left_included , right_included,all_included))
    ]
  names(left_excluded) <- sub("\\_left$", "", names(left_excluded)) %0% NULL
  names(right_excluded) <- sub("\\_right$", "", names(right_excluded)) %0% NULL
  names(upper_excluded) <- sub("\\_upper$", "", names(upper_excluded)) %0% NULL
  names(lower_excluded) <- sub("\\_lower$", "", names(lower_excluded)) %0% NULL

  setNames(
    purrr::map(
      list(upper_excluded, lower_excluded, left_excluded, right_excluded,
           upper_included, lower_included, left_included , right_included,
           all_included, all_excluded),
      make_tex_opts),
    c("upper_excluded", "lower_excluded", "left_excluded", "right_excluded",
      "upper_included", "lower_included", "left_included", "right_included",
      "all_included", "all_excluded")
  )
}

latexify_node <- function(.node, .dag) {

  node_id <- paste0("(", .node$id, ") ")

  math <- .node$adorn_math %||% .dag$adorn_math
  node_text <- if (math) paste0("{$", .node$name, "$}") else
    paste0("{", .node$name, "}")

  node_opts <- if (.node$is_swig) split_swig_opts(.node$options) else
    make_tex_opts(.node$options)

  shape <- .node$options[["shape"]] %||% .dag$node_options[["shape"]]
  draw <- !is.null(shape)

  # only deal with regular (non-swig specific options) here
  if (.node$is_swig) {

    shape <- .node$options[["shape"]] %||% .dag$swig_options[["shape"]]
    draw <- !is.null(shape)

    node_split <- NULL
    split <- .node$options$split %||% .dag$swig_options$split
    split <- split %||% "v"
    if (!draw) {
      node_opts$all_excluded <- paste0("opacity=0, text opacity=1", node_opts$all_excluded)
      # to override the solid black if desired in the node splitting line
      # other node opts get passed outside of this if pertinent
      special_opts <- .node$options[names(.node$options) %in%
                                  c("color", "line_type", "line_width")]

      node_split <- ifelse(split == "v",
                           paste0("\n\\draw[very thick, solid, black,",
                                  make_tex_opts(special_opts),
                                  "] (", .node$id,
                                  ".left north) -- (", .node$id,".left south);"),
                           paste0("\n\\draw[very thick, solid, black,",
                                  make_tex_opts(special_opts),
                                  "] (", .node$id,
                                  ".west) -- (", .node$id,".east);"))
    } else if (!shape %in% c("ellipse", "circle", "circle part")) warning("Shape not available for SWIG node; defaulting to ellipse.")
  }

  # for all nodes
  compiled_options <- paste0(ifelse(draw, "draw,", ""),
                             .node$position, ",")

  # now deal with swig specific options, i.e. left/right excluded and all included
  if (.node$is_swig) {

    main_part <- paste0(compiled_options,
                        ", ", node_opts$all_excluded,
                        ", shape=swig ", split, "split, text=",
                        .node$options[["text"]] %||% .dag$swig_options[["text"]] %||% "black",
                        ", swig ", split, "split={",
                        ifelse(split == "v", node_opts$left_included, node_opts$upper_included),
                        ",",
                        ifelse(split == "v", node_opts$right_included, node_opts$lower_included),
                        ", ",
                        node_opts$all_included, "}]{")

    left_part <- paste0("\\nodepart[",
                        ifelse(split == "v", "l", "up"),
                        ",",
                        ifelse(split == "v", node_opts$left_excluded, node_opts$upper_excluded),
                        "]{",
                        ifelse(split == "v", "left", "upper"),
                        "}{",
                        if (math) "$",
                        .node$name[1],
                        if (math) "$",
                        "}")
    right_part <- paste0("\\nodepart[",
                        ifelse(split == "v", "r", "lo"),
                        ",",
                        ifelse(split == "v", node_opts$right_excluded, node_opts$lower_excluded),
                        "]{",
                        ifelse(split == "v", "right", "lower"),
                        "}{",
                        if (math) "$",
                        .node$name[2],
                        if (math) "$",
                        "}}")
    all_parts <- paste0("\\node[name=",.node$id, ",",
                        main_part, left_part,right_part, ";",
                        node_split)
    return(all_parts)

  } else {

    compiled_options <- paste0(compiled_options, node_opts,
                               ", text=",
                               .node$options[["text"]] %||% .dag$node_options[["text"]] %||% "black",
                               "] ")


    node_to_paste <- paste0("\\node[", compiled_options, node_id,
                            ifelse(is.null(.node$position),
                                   paste0("at (", .node$coords[1],
                                          ",", .node$coords[2], ") "), ""),
                            node_text, " ;")

    return(node_to_paste)
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


