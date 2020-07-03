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
    dagtex.swig_options = NULL,
    dagtex.node_options = NULL,
    dagtex.edge_options = NULL,
    dagtex.cleanup = c("aux", "log", "txt", "Doc", "tex"),
    dagtex.shape = NULL
    )
  toset <- !(names(op.dagtex) %in% names(op))
  if (any(toset)) options(op.dagtex[toset])

  invisible()

}
