library(ThesisPackage)
library(ggplot2)
library(cowplot)

# BEAST phylogeography ----
beastGeography <-
  system.file("extdata/BEAST/phylogeography.mcc", package = "ThesisPackage") %>%
  read_tree(tree_file = .)

legends <-
  plot_grid(
    NULL,
    beast_legend_color(tree_data = beastGeography),
    beast_legend_probability(tree_data = beastGeography),
    nrow = 1, ncol = 3, rel_widths = c(0.25, 1, .75)
  )

FigResultsBeastGeography <-
  plot_grid(
    NULL,
    beast_plot(tree_data = beastGeography, ggtree_layout = "circular") +
      ggtree::xlim_expand(xlim = c(0, 0.00325), panel = "Tree") +
      theme(
        legend.position = "none"
      ),
    NULL, legends, NULL,
    nrow = 5, rel_heights = c(0.1, 2, 0.2, 0.5, 0.2)
  )

ThesisPackage::save_plot(
  gg_plot = FigResultsBeastGeography,
  width = 6, height = 7.5
)
