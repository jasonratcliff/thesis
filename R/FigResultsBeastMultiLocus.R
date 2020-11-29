library(ThesisPackage)
library(ggplot2)
library(cowplot)

# BEAST multi ----
beastMultiLocus <-
  system.file("extdata/BEAST/multi-locus.combined.mcc",
              package = "ThesisPackage") %>%
  read_tree(tree_file = .)

legends <-
  plot_grid(
    NULL,
    beast_legend_color(tree_data = beastMultiLocus),
    beast_legend_probability(tree_data = beastMultiLocus),
    nrow = 1, ncol = 3, rel_widths = c(0.25, 1, .75)
  )

FigResultsBeastMultiLocus <-
  plot_grid(NULL,
    beast_plot(tree_data = beastMultiLocus, ggtree_layout = "circular") +
      theme(
        legend.position = "none"
      ),
    NULL, legends, NULL,
    nrow = 5, rel_heights = c(0.1, 0.5, 0.1, 0.2, 0.1)
  )

ThesisPackage::save_plot(
  gg_plot = FigResultsBeastMultiLocus,
  height = 8, width = 6
)
