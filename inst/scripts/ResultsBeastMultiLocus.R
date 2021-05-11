library(Thesis)
library(ggplot2)
library(cowplot)

# BEAST multi ----

ggtree_beast <- list()

ggtree_beast$data <-
  system.file(
    "extdata/BEAST/multi-locus.combined.mcc",
    package = "Thesis"
  ) %>%
  read_tree(tree_file = .)

ggtree_beast$plot <-
  beast_plot(tree_data = ggtree_beast$data, ggtree_layout = "circular") +
  theme(plot.margin = margin(0.2, 0.2, 0.3, 0.2, "in"))

# png ----

ggtree_beast$legend_png <-
  plot_grid(
    plotlist = list(
      beast_legend_color(
        tree_data = ggtree_beast$data,
        ncol = 1
      ),
      beast_legend_probability(tree_data = ggtree_beast$data)
    ),
    nrow = 2, ncol = 1, rel_widths = c(0.5, 0.5)
  )

ggtree_beast$png <-
  plot_grid(
    ggtree_beast$plot,
    ggtree_beast$legend_png,
    nrow = 1, rel_widths = c(0.75, 0.25)
  )

# pdf ----

ggtree_beast$legend_pdf <-
  plot_grid(
    NULL,
    beast_legend_color(tree_data = ggtree_beast$data),
    beast_legend_probability(tree_data = ggtree_beast$data),
    nrow = 1, ncol = 3, rel_widths = c(0.25, 1, .75)
  )

ggtree_beast$pdf <-
  plot_grid(
    NULL,
    ggtree_beast$plot,
    ggtree_beast$legend_pdf,
    nrow = 3, rel_heights = c(0.05, 0.65, 0.3)
  )

# plot grids ----

purrr::pwalk(
  .l = list(
    ext = c("png", "pdf"),
    plot = list(ggtree_beast$png, ggtree_beast$pdf),
    width = c(6, 6),
    height = c(8, 4),
    aspect = c(.167, 2.5),
    row = c(1, 2),
    col = c(2, 1)
  ),
  .f = function(ext, plot, width, height, aspect, row, col) {
    cowplot::save_plot(
      filename = fs::path("inst/figures/ResultsBeastMultiLocus", ext = ext),
      plot = plot,
      base_width = width,
      base_height = height,
      base_asp = aspect,
      nrow = row,
      ncol = col
    )
  })

