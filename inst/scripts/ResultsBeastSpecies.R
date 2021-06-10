library(Thesis)
library(ggplot2)
library(ggtree)
library(ggrepel)
library(cowplot)

# Hypothesis 1 ----
spp_hypothesis_1 <-
  treeio::read.beast(
    file = system.file(
      "extdata/BEAST/spp-hypothesis-1.mcc", package = "Thesis"
    )
  ) %>%
  ggtree::fortify() %>%
  Thesis::species_plot(tree_data = .) +
  ggplot2::ggtitle(label = "Hypothesis 1")

# Hypothesis 2 ----
spp_hypothesis_2 <-
  treeio::read.beast(
    file = system.file(
      "extdata/BEAST/spp-hypothesis-2.mcc", package = "Thesis"
    )
  ) %>%
  ggtree::fortify() %>%
  Thesis::species_plot(tree_data = .) +
  ggplot2::ggtitle(label = "Hypothesis 2")

# Hypothesis 3 ----
spp_hypothesis_3 <-
  treeio::read.beast(
    file = system.file(
      "extdata/BEAST/spp-hypothesis-3.mcc", package = "Thesis"
    )
  ) %>%
  ggtree::fortify() %>%
  Thesis::species_plot(tree_data = .) +
  ggplot2::ggtitle(label = "Hypothesis 3")

# Hypothesis 4 ----
spp_hypothesis_4 <-
  treeio::read.beast(
    file = system.file(
      "extdata/BEAST/spp-hypothesis-4.mcc", package = "Thesis"
    )
  ) %>%
  ggtree::fortify() %>%
  Thesis::species_plot(tree_data = .) +
  ggplot2::ggtitle(label = "Hypothesis 4")

# cowplot Grid ----

ggtree_spp <- list()
ggtree_spp$theme <-
  list(
    ggplot2::theme(
      legend.position = "none"
    )
  )

ggtree_spp$spp <-
  cowplot::plot_grid(
    spp_hypothesis_1 + ggtree_spp$theme,
    spp_hypothesis_2 + ggtree_spp$theme,
    spp_hypothesis_3 + ggtree_spp$theme,
    spp_hypothesis_4 + ggtree_spp$theme,
    nrow = 2
  )

ggtree_spp$plot <-
  cowplot::plot_grid(
    ggtree_spp$spp, cowplot::get_legend(spp_hypothesis_1),
    ncol = 2, rel_widths = c(0.9, 0.1)
  )

purrr::pwalk(
  .l = list(
    ext = c("png", "pdf"),
    plot = list(ggtree_spp$plot, ggtree_spp$plot),
    width = c(6, 6),
    height = c(8, 4),
    aspect = c(.167, 2.5),
    row = c(1, 2),
    col = c(2, 1)
  ),
  .f = function(ext, plot, width, height, aspect, row, col) {
    cowplot::save_plot(
      filename = fs::path("inst/figures/ResultsBeastSpecies", ext = ext),
      plot = plot,
      base_width = width,
      base_height = height,
      base_asp = aspect,
      nrow = row,
      ncol = col
    )
  })

