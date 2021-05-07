library(ThesisPackage)
library(cowplot)
library(ggplot2)
library(ggtree)
library(magrittr)
library(purrr)

# Results: MrBayes rps ggtree plot

ggtree_rps <- list()

ggtree_rps$joined <-
  list.files(
    path = system.file("extdata/MrBayes", package = "ThesisPackage"),
    pattern = "rps-infile.nex.con.tre",
    full.names = TRUE
  ) %>%
  read_tree(tree_file = .) %>%
  join_bayes(
    tree_data = .,
    id_column = "prior_id",
    scale_vector = c(4, 12)
  )

ggtree_rps$tree <-
  bayes_ggtree(
    joined_ggtree = ggtree_rps$joined,
    id_column = "prior_id",
    ggtree_layout = "rectangular"
  ) +
  bayes_themes(
    joined_ggtree = ggtree_rps$joined,
    id_column = "prior_id"
  )

ggtree_rps$plot <- ggtree_rps$tree +
  theme(legend.position = "none") +
  expand_limits(x = 0.013)

# png ----

ggtree_rps$legend_png <-
  get_legend(
    ggtree_rps$tree +
      guides(
        color = guide_legend(ncol = 1, override.aes = list(size = 5))
      ) +
      labs(colour = "Prior Annotation", shape = "Prior Annotation")
  )

ggtree_rps$png <-
  plot_grid(
    ggtree_rps$plot,
    ggtree_rps$legend_png,
    nrow = 1,
    rel_widths = c(2, 1)
  )

# pdf ----

ggtree_rps$legend_pdf <-
  get_legend(
    ggtree_rps$tree +
      guides(
        color = guide_legend(ncol = 2, override.aes = list(size = 5))
      ) +
      labs(colour = "Prior Annotation", shape = "Prior Annotation")
  )

ggtree_rps$pdf <- cowplot::ggdraw(ggtree_rps$plot) +
  cowplot::draw_plot(
    plot = ggtree_rps$legend_pdf,
    x = 0.15, y = 0.55,
    width = 0.25, height = 0.425
  )

# plot grids ----

purrr::pwalk(
  .l = list(
    ext = c("png", "pdf"),
    plot = list(ggtree_rps$png, ggtree_rps$pdf),
    width = c(6, 6),
    height = c(8, 4),
    aspect = c(.167, 2.5),
    row = c(1, 2),
    col = c(2, 1)
  ),
  .f = function(ext, plot, width, height, aspect, row, col) {
    cowplot::save_plot(
      filename = fs::path("inst/figures/ResultsBayes_rps", ext = ext),
      plot = plot,
      base_width = width,
      base_height = height,
      base_asp = aspect,
      nrow = row,
      ncol = col
    )
  })

