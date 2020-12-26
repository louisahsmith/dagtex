#' Install Thomas Richardson's tikz library for drawing SWIGs
#' @param ...
#' @details
#' Puts the swigs tikz library into the user's texmf path. This file either
#' needs to be manually installed, as with this function, or copied from
#' `system.file("tex", "pgflibraryshapes.swigs.code.tex", package = "dagtex")`
#' to the working directory whenever a SWIG is created in an RMarkdown document.
#' Not necessary if not creating SWIGs, or if only previewing in the viewer and
#' not via RMarkdown.
#' @export
#' @rdname use_swigs

install_swigs <- function(...) {
  tryCatch({
    val <- system("tlmgr conf texmf TEXMFHOME", intern = TRUE)
    dir <- regexpr("\\(.+\\)", val)
    short_dir <- substr(val, dir + 1, dir + attr(dir, "match.length") - 2)
    short_dir <- substr(short_dir, 1, rev(gregexpr("\\/", short_dir)[[1]])[1] - 1)
    full_dir <- file.path(short_dir, "texmf-dist", "tex", "latex", "local")
    if (!dir.exists(full_dir)) {
      dir.create(full_dir)
    }
    file.copy(system.file("tex", "pgflibraryshapes.swigs.code.tex", package = "dagtex"),
              file.path(full_dir, "pgflibraryshapes.swigs.code.tex"))
    system("texhash")
  }, error = function(e) e,
  finally = cat("Error installing tikz library for SWIGs. You may need to do so manually.\n"))
}


#' Use Thomas Richardson's tikz library for drawing SWIGs
#' @param path The directory with the RMarkdown file that uses SWIGs
#' @details
#' Copies the swigs tikz library into the specified directory, creating a file
#' "pgflibraryshapes.swigs.code.tex" to be used when creating SWIGs via
#' RMarkdown document.
#' @export
#' @rdname use_swigs

use_swigs <- function(path = getwd()) {
  file.copy(
    system.file("tex", "pgflibraryshapes.swigs.code.tex", package = "dagtex"),
    path
    )
}
