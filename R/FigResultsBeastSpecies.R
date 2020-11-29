library(ThesisPackage)
library(ggplot2)
library(ggtree)
library(ggrepel)
library(cowplot)

# Hypothesis 1 ----
spp_hypothesis_1 <-
  treeio::read.beast(
    file = system.file(
      "extdata/BEAST/spp-hypothesis-1.mcc", package = "ThesisPackage"
    )
  ) %>%
  ggtree::fortify() %>%
  species_plot(tree_data = .) +
  ggtitle("Hypothesis 1")

# Hypothesis 2 ----
spp_hypothesis_2 <-
  treeio::read.beast(
    file = system.file(
      "extdata/BEAST/spp-hypothesis-2.mcc", package = "ThesisPackage"
    )
  ) %>%
  ggtree::fortify() %>%
  species_plot(tree_data = .) +
  ggtitle("Hypothesis 2")

# Hypothesis 3 ----
spp_hypothesis_3 <-
  treeio::read.beast(
    file = system.file(
      "extdata/BEAST/spp-hypothesis-3.mcc", package = "ThesisPackage"
    )
  ) %>%
  ggtree::fortify() %>%
  species_plot(tree_data = .) +
  ggtitle("Hypothesis 3")

# Hypothesis 4 ----
spp_hypothesis_4 <-
  treeio::read.beast(
    file = system.file(
      "extdata/BEAST/spp-hypothesis-4.mcc", package = "ThesisPackage"
    )
  ) %>%
  ggtree::fortify() %>%
  species_plot(tree_data = .) +
  ggtitle("Hypothesis 4")

# cowplot Grid ----
spp_plots <-
  plot_grid(
    spp_hypothesis_1 + theme(legend.position = "none"),
    spp_hypothesis_2 + theme(legend.position = "none"),
    spp_hypothesis_3 + theme(legend.position = "none"),
    spp_hypothesis_4 + theme(legend.position = "none"),
    nrow = 2
  )

FigResultsBeastSpecies <- 
  plot_grid(
    spp_plots, get_legend(spp_hypothesis_1),
    ncol = 2, rel_widths = c(0.9, 0.1)
  )

ThesisPackage::save_plot(
  gg_plot = FigResultsBeastSpecies,
  width = 6, height = 6
)
