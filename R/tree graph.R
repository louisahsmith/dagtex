#' @export
tree_graph <- function(tree_tab = NULL, n = NULL, EY = NULL,
                       probabilities = FALSE, fractions = FALSE, digits = 2,
                       x1 = 0, y1 = 0, x2 = 3, y2 = c(1.5, -1.5), x3 = 6,
                       y3 = seq(3, -3, length.out = 8), x4 = 7.5, y4 = y3,
                       x5 = 8.5, y5 = y3, x6 = 9.25, y6 = y3, x7 = 10, y7 = y3,
                       xtop = c(x1 + 1.25, x2, x3 - 1.5, x4 - .65, x5 , x6, x7),
                       ytop = max(y7 + .75),
                       n_annotate = TRUE, A0_annotate = TRUE, L1_annotate = TRUE,
                       A1_annotate = TRUE, EY_annotate = TRUE,
                       A0_yshift = -10, A0_xshift = 0,
                       L1_yshift = 4, L1_xshift = -12,
                       A1_yshift = 7.5, A1_xshift = 0,
                       circle_width = 50, ...) {

  circle_width <- paste0(circle_width, "pt")
  n <- purrr::`%||%`(tree_tab$n, n)
  if (is.null(n)) stop("No values provided for n")
  EY <- purrr::`%||%`(tree_tab$EY, EY)
  if (is.null(n)) stop("No values provided for EY")

  a0_vals <- purrr::`%||%`(tree_tab$A0, rep(c("0", "1"), each = 4))
  l1_vals <- purrr::`%||%`(tree_tab$L1, rep(rep(c("0", "1"), each = 2), 2))
  a1_vals <- purrr::`%||%`(tree_tab$A1, rep(c("0", "1"), 4))

  a1_start_positions <- rep(rep(c("north east", "south east"), each = 2), 2)
  l1_nodes <-  rep(2:3, each = 4)
  a1_nodes <- 4:11
  EY_nodes <- 12:19

  N <- sum(n)
  A0a <- sum(n[1:4])
  A0b <- sum(n[1:4])
  L1a <- n[1] + n[2]
  L1b <- n[3] + n[4]
  L1c <- n[5] + n[6]
  L1d <- n[7] + n[8]

  A0_label <- "A_0"
  L1_label <- "L_1"
  A1_label <- "A_1"

  if (probabilities) {
    A0_label <- "f(A_0)"
    L1_label <- "f(L_1 \\mid A_0)"
    A1_label <- "f(A_1 \\mid A_0, L_1)"
    xtop[2] <- xtop[2] - .35
    A1_yshift <- A1_yshift - 1
    A0_yshift <- A0_yshift + 6
    n[1:2] <- n[1:2] / L1a
    n[3:4] <- n[3:4] / L1b
    n[5:6] <- n[5:6] / L1c
    n[7:8] <- n[7:8] / L1d
    L1a <- L1a/A0a
    L1b <- L1b/A0a
    L1c <- L1c/A0b
    L1d <- L1d/A0b
    A0a <- A0a/N
    A0b <- A0b/N
    N <- N/N
    if (fractions) {
      A1_yshift <- A1_yshift - 2
      n <- paste(MASS::fractions(n))
      L1a <- paste(MASS::fractions(L1a))
      L1b <- paste(MASS::fractions(L1b))
      L1c <- paste(MASS::fractions(L1c))
      L1d <- paste(MASS::fractions(L1d))
      A0a <- paste(MASS::fractions(A0a))
      A0b <- paste(MASS::fractions(A0b))
    } else {
      n <- round(n, digits = digits)
      L1a <- round(L1a, digits = digits)
      L1b <- round(L1b, digits = digits)
      L1c <- round(L1c, digits = digits)
      L1d <- round(L1d, digits = digits)
      A0a <- round(A0a, digits = digits)
      A0b <- round(A0b, digits = digits)
    }
  }


  A0_positions = rep(paste0("sloped, yshift=", A0_yshift, "pt, xshift=",
                            A0_xshift, "pt, above"), 2)
  L1_positions = rep(paste0("sloped, yshift=", L1_yshift, "pt, xshift=",
                            L1_xshift, "pt, above, anchor=west"), 4)
  A1_positions = paste0("sloped, yshift = ", rep(c("-", ""), 4),
                        A1_yshift, "pt, xshift = ", A1_xshift, "pt, ",
                        rep(c("above", "below"), 4))

  # base
  tree <- dagtex(.node_options = list(inner_sep = "0.01pt", shape = "circle"),
                 adorn_math = FALSE, ...) %>%
    # node 1
    add_node(ifelse(n_annotate, N, ""), x = x1, y = y1,
             .options = list(shape = "circle split", minimum_width = circle_width)) %>%
    # node 2: L1-a
    add_node(" ", x = x2, y = y2[1],
             .options = list(shape = "circle", minimum_width = circle_width)) %>%
    # node 3: L1-b
    add_node(" ", x = x2, y = y2[2],
             .options = list(shape = "circle", minimum_width = circle_width)) %>%

    # A0 numbers
    add_edge(1, 2, start_position = "east", end_position = "west", is_headless = TRUE,
             annotate = annotate_edge(ifelse(A0_annotate, A0a, ""),
                                      position = A0_positions[1])) %>%
    add_edge(1, 3, start_position = "east", end_position = "west", is_headless = TRUE,
             annotate = annotate_edge(ifelse(A0_annotate, A0b, ""),
                                      position = A0_positions[2])) %>%

    # L1 numbers
    add_edge(2, 2, start_position = "west", end_position = "north east", is_headless = TRUE,
             annotate = annotate_edge(ifelse(L1_annotate, L1a, ""),
                                      position = L1_positions[1])) %>%
    add_edge(2, 2, start_position = "west", end_position = "south east", is_headless = TRUE,
             annotate = annotate_edge(ifelse(L1_annotate, L1b, ""),
                                      position = L1_positions[1])) %>%
    add_edge(3, 3, start_position = "west", end_position = "north east", is_headless = TRUE,
             annotate = annotate_edge(ifelse(L1_annotate, L1c, ""),
                                      position = L1_positions[1])) %>%
    add_edge(3, 3, start_position = "west", end_position = "south east", is_headless = TRUE,
             annotate = annotate_edge(ifelse(L1_annotate, L1d, ""),
                                      position = L1_positions[4]))


  # hidden nodes where dashed lines start
  # nodes 4:11
  for (i in 1:8) {
    tree <- tree %>%
      add_node(sprintf("%03s", EY[i]), x = x3, y = y3[i],
               .options = list(draw = "none", text = "white",
                               font = "\\footnotesize"))
  }

  # E[Y] values
  # nodes 12:19
  for (i in 1:8) {
    tree <- tree %>%
      add_node(paste0(EY[i]), x = x4, y = y4[i],
               .options = list(draw = "none", anchor = "west",
                               text = ifelse(EY_annotate, "black", "white")))
  }

  # A1 n annotations
  for (i in 1:8) {
    tree <- tree %>%
      add_edge(l1_nodes[i], a1_nodes[i], start_position = a1_start_positions[i],
               end_position = "west", is_headless = TRUE,
               annotate = annotate_edge(n[i], position = A1_positions[i]))
  }

  # dashed lines
  for (i in 1:8) {
    tree <- tree %>%
      add_edge(a1_nodes[i], EY_nodes[i], start_position = "west",
               end_position = "west", is_headless = TRUE,
               .options = list(line_type = "dashed"))
  }

  # A0, L1, A1 table
  # nodes 20, 21, 22
  # ..... 41, 42, 43
  for (i in 1:8) {
    tree <- tree %>%
      add_node(a0_vals[i], x = x5, y = y5[i], .options = list(draw = "none")) %>%
      add_node(l1_vals[i], x = x6, y = y6[i], .options = list(draw = "none")) %>%
      add_node(a1_vals[i], x = x7, y = y7[i], .options = list(draw = "none"))
  }

  # labels on top row
  # nodes 44:50
  tree <- tree %>%
    add_node(A0_label, x = xtop[1], y = ytop, adorn_math = TRUE,
             .options = list(draw = "none", font = "\\small")) %>%
    add_node(L1_label, x = xtop[2], y = ytop, adorn_math = TRUE,
             .options = list(draw = "none", font = "\\small")) %>%
    add_node(A1_label, x = xtop[3], y = ytop, adorn_math = TRUE,
             .options = list(draw = "none", font = "\\small")) %>%
    add_node("E[Y\\mid A_0, L_1, A_1]", x = xtop[4], y = ytop, adorn_math = TRUE,
             .options = list(draw = "none", font = "\\small")) %>%
    add_node("A_0", x = xtop[5], y = ytop, adorn_math = TRUE,
             .options = list(draw = "none", font = "\\small")) %>%
    add_node("L_1", x = xtop[6], y = ytop, adorn_math = TRUE,
             .options = list(draw = "none", font = "\\small")) %>%
    add_node("A_1", x = xtop[7], y = ytop, adorn_math = TRUE,
             .options = list(draw = "none", font = "\\small"))

  tree
}
