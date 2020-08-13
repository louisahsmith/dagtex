.onLoad <- function(libname = find.package("dagtex"), pkgname = "dagtex") {

  load_packages <- getOption("dagtex.load_tikz", default = TRUE)
  if (load_packages) {
    knitr::knit_meta_add(
      list(
        rmarkdown::latex_dependency_tikz(
          c("positioning",
            "calc",
            "shapes.geometric",
            "shapes.multipart",
            "shapes",
            "arrows.meta",
            "arrows",
            "shapes.arrows",
            "decorations.markings",
            "external",
            "trees")
        )
      )
    )
  }

  op <- options()
  op.dagtex <- list(
    dagtex.adorn_math = FALSE,
    dagtex.help_lines = FALSE,
    dagtex.help_arrows = FALSE,
    dagtex.density = 320,
    dagtex.swig_options = NA,
    dagtex.node_options = NA,
    dagtex.edge_options = NA,
    dagtex.cleanup = c("aux", "log", "txt", "Doc", "tex"),
    dagtex.shape = NA,
    dagtex.notation = "superscript"
    )
  toset <- !(names(op.dagtex) %in% names(op))
  if (any(toset)) options(op.dagtex[toset])

  invisible()

}
