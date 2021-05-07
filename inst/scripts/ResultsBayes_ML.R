library(ThesisPackage)
library(cowplot)
library(ggplot2)
library(ggtree)
library(magrittr)
library(purrr)

# Results: MrBayes multi-locus ggtree plot

ggtree_ML <- list()

ggtree_ML$joined <-
  list.files(
    path = system.file("extdata/MrBayes", package = "ThesisPackage"),
    pattern = "ml-infile.nex.con.tre",
    full.names = TRUE
  ) %>%
  read_tree(tree_file = .) %>%
  join_bayes(
    tree_data = .,
    id_column = "prior_id",
    scale_vector = c(4, 8)
  )

ggtree_ML$tree <-
  bayes_ggtree(
    joined_ggtree = ggtree_ML$joined,
    id_column = "prior_id",
    ggtree_layout = "rectangular"
  ) +
  bayes_themes(
    joined_ggtree = ggtree_ML$joined,
    id_column = "prior_id"
  )

ggtree_ML$plot <- ggtree_ML$tree +
  theme(legend.position = "none") +
  expand_limits(x = 0.022)

# png ----

ggtree_ML$legend_png <-
  get_legend(
    ggtree_ML$tree +
      guides(
        color = guide_legend(ncol = 1, override.aes = list(size = 5))
      ) +
      labs(colour = "Prior Annotation", shape = "Prior Annotation")
  )

ggtree_ML$png <-
  plot_grid(
    ggtree_ML$plot,
    ggtree_ML$legend_png,
    nrow = 1,
    rel_widths = c(2, 1)
  )

# pdf ----

ggtree_ML$legend_pdf <-
  get_legend(
    ggtree_ML$tree +
      guides(
        color = guide_legend(ncol = 2, override.aes = list(size = 5))
      ) +
      labs(colour = "Prior Annotation", shape = "Prior Annotation")
  )

ggtree_ML$pdf <-
  cowplot::ggdraw(ggtree_ML$plot) +
  cowplot::draw_plot(
    plot = ggtree_ML$legend_pdf,
    x = 0.15, y = 0.45,
    width = 0.25, height = 0.425
  )

# plot grids ----

purrr::pwalk(
  .l = list(
    ext = c("png", "pdf"),
    plot = list(ggtree_ML$png, ggtree_ML$pdf),
    width = c(6, 6),
    height = c(8, 4),
    aspect = c(.167, 2.5),
    row = c(1, 2),
    col = c(2, 1)
  ),
  .f = function(ext, plot, width, height, aspect, row, col) {
    cowplot::save_plot(
      filename = fs::path("Figs/FigResultsBayes_ML", ext = ext),
      plot = plot,
      base_width = width,
      base_height = height,
      base_asp = aspect,
      nrow = row,
      ncol = col
    )
  })

