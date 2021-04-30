library(ThesisPackage)
library(cowplot)
library(ggplot2)
library(ggtree)
library(magrittr)
library(purrr)

# Results: MrBayes rITS ggtree plot

ggtree_rITS <- list()

ggtree_rITS$joined <-
  list.files(
    path = system.file("extdata/MrBayes", package = "ThesisPackage"),
    pattern = "rITS-infile.nex.con.tre",
    full.names = TRUE
  ) %>%
  read_tree(tree_file = .) %>%
  join_bayes(
    tree_data = .,
    id_column = "prior_id",
    scale_vector = c(5, 10)
  )

ggtree_rITS$tree <-
  bayes_ggtree(
    joined_ggtree = ggtree_rITS$joined,
    id_column = "prior_id",
    ggtree_layout = "rectangular"
  ) +
    bayes_themes(
      joined_ggtree = ggtree_rITS$joined,
      id_column = "prior_id"
    )

ggtree_rITS$plot <- ggtree_rITS$tree +
  theme(legend.position = "none") +
  expand_limits(x = 0.025)

# png ----

ggtree_rITS$legend_png <-
  get_legend(
    ggtree_rITS$tree +
      guides(
        color = guide_legend(ncol = 1, override.aes = list(size = 5))
      ) +
      labs(colour = "Prior Annotation", shape = "Prior Annotation")
  )

ggtree_rITS$png <-
  plot_grid(
    ggtree_rITS$plot,
    ggtree_rITS$legend_png,
    nrow = 1,
    rel_widths = c(2, 1)
  )

# pdf ----

ggtree_rITS$legend_pdf <-
  get_legend(
    ggtree_rITS$tree +
      guides(
        color = guide_legend(ncol = 2, override.aes = list(size = 5))
      ) +
      labs(colour = "Prior Annotation", shape = "Prior Annotation")
  )

ggtree_rITS$pdf <- cowplot::ggdraw(ggtree_rITS$plot) +
  cowplot::draw_plot(
    plot = ggtree_rITS$legend_pdf,
    x = 0.125, y = 0.425,
    width = 0.25, height = 0.5
  )

# plot grids ----

purrr::pwalk(
  .l = list(
    ext = c("png", "pdf"),
    plot = list(ggtree_rITS$png, ggtree_rITS$pdf),
    width = c(6, 6),
    height = c(8, 4),
    aspect = c(.167, 2.5),
    row = c(1, 2),
    col = c(2, 1)
  ),
  .f = function(ext, plot, width, height, aspect, row, col) {
    cowplot::save_plot(
      filename = fs::path("Figs/FigResultsBayes_rITS", ext = ext),
      plot = plot,
      base_width = width,
      base_height = height,
      base_asp = aspect,
      nrow = row,
      ncol = col
    )
  })

