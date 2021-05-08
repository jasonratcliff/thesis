library(ThesisPackage)
library(ggplot2)
library(cowplot)

# BEAST phylogeography ----

ggtree_geo <- list()

ggtree_geo$data <-
  system.file(
    "extdata/BEAST/phylogeography.mcc",
    package = "ThesisPackage"
  ) %>%
  read_tree(tree_file = .)

ggtree_geo$plot <-
  beast_plot(tree_data = ggtree_geo$data, ggtree_layout = "circular") +
  theme(plot.margin = margin(0.2, 0.2, 0.3, 0.2, "in"))

# png ----

ggtree_geo$legend_png <-
  plot_grid(
    plotlist = list(
      beast_legend_color(
        tree_data = ggtree_geo$data,
        ncol = 1
      ),
      beast_legend_probability(tree_data = ggtree_geo$data)
    ),
    nrow = 2, ncol = 1, rel_widths = c(0.5, 0.5)
  )

ggtree_geo$png <-
  plot_grid(
    ggtree_geo$plot,
    ggtree_geo$legend_png,
    nrow = 1, rel_widths = c(0.75, 0.25)
  )

# pdf ----

ggtree_geo$legend_pdf <-
  plot_grid(
    NULL,
    beast_legend_color(tree_data = ggtree_geo$data),
    beast_legend_probability(tree_data = ggtree_geo$data),
    nrow = 1, ncol = 3, rel_widths = c(0.25, 1, .75)
  )

ggtree_geo$pdf <-
  plot_grid(
    NULL,
    ggtree_geo$plot,
    ggtree_geo$legend_pdf,
    nrow = 3, rel_heights = c(0.05, 0.65, 0.3)
  )

# plot grids ----

purrr::pwalk(
  .l = list(
    ext = c("png", "pdf"),
    plot = list(ggtree_geo$png, ggtree_geo$pdf),
    width = c(6, 6),
    height = c(8, 4),
    aspect = c(.167, 2.5),
    row = c(1, 2),
    col = c(2, 1)
  ),
  .f = function(ext, plot, width, height, aspect, row, col) {
    cowplot::save_plot(
      filename = fs::path("inst/figures/ResultsBeastGeography", ext = ext),
      plot = plot,
      base_width = width,
      base_height = height,
      base_asp = aspect,
      nrow = row,
      ncol = col
    )
  })

