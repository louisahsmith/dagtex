#' Create a new DAG
#'
#' @param .node_options List of options that apply to each node (can be
#'   overwritten for individual nodes).
#' @param .edge_options List of options that apply to each edge (can be
#'   overwritten for individual edges).
#' @param .swig_options List of options that apply to each SWIG node. Currently
#'   cannot be overwritten individually.
#' @return
#' @details
#'
#' For (non-SWIG) nodes, shape options are "rectangle", "circle", "ellipse",
#' "circle split", "forbidden sign", "diamond", "cross out", "strike out",
#' "regular polygon", "star".
#'
#' SWIG nodes take the following possible options: gap, fill_color_left,
#' line_color_left, line_width_left, inner_line_width_left.
#'
#' Color values can be any of: "red", "green", "blue", "cyan", "magenta",
#' "yellow", "black", "gray",  "darkgray", "lightgray", "brown", "lime",
#' "olive", "orange", "pink",  "purple", "teal", "violet", "white".
#'
#' Other options should be named as in Tikz code, with "_" (underscores) replacing
#' spaces. A resource for these options is here:
#' (https://en.wikibooks.org/wiki/LaTeX/PGF/TikZ).
#'
#' Line type ("solid", "dashed", etc.) is denoted with `linetype =`.
#'
#' @export
#'
#' @examples
#' dagtex(
#'   .node_options = list(shape = "star"),
#'   .swig_options = list(gap = "3pt", line_color_right = "red",
#'                        fill_color_left = "pink", line_width_left = 2.75),
#'   .edge_options = list(linetype = "dashed", color = "green")
#' ) %>%
#'   add_node("THIS IS A SWIG",
#'            .options = list(shape = "forbidden sign", color = "blue",
#'                            line_width = 4)
#'            ) %>%
#'   add_swig_node(.left = "$X$", .right = "$x = 1$") %>%
#'   add_node("$Y^{x = 1}$",
#'            .options = list(color = "violet", text = "olive")
#'            ) %>%
#'   add_edge("$x = 1$", "$Y^{x = 1}$",
#'            curve = "up", is_double_arrow = TRUE,
#'            .options = list(color = "teal", linetype = "solid")
#'            ) %>%
#'   add_edge(1, 3,
#'            curve_in_degree = 0, curve_out_degree = 40
#'            ) %>%
#'   add_node("hello", above = 2,
#'            .options = list(fill = "yellow"))
#'
dagtex <- function(.node_options = NULL, .edge_options = NULL, .swig_options = NULL,
                   help_lines = FALSE, help_angles = FALSE, adorn_math = FALSE, ...) {
  structure(
    list(
      nodes = list(),
      edges = list(),
      latex = list(),
      node_options = .node_options,
      edge_options = .edge_options,
      swig_options = .swig_options,
      help_lines = help_lines,
      help_angles = help_angles,
      adorn_math = adorn_math,
      texPreview_options = list(...)
    ),
    class = "dagtex"
  )
}


