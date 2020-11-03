library(ThesisPackage)
library(cowplot)
library(ggplot2)

# Results: ggplot distribution of specimen Basal Leaf Margins.
ggplotTraitMargins <- ThesisPackage::trait_margins %>%
  map_trait_distribution(tidy_trait = .) +
  scale_color_viridis_d(option = "A") +
  labs(color = "Basal Leaf Margins")

FigResultsTraitMargins <-
  plot_grid(
    ggplotTraitMargins +
      theme(legend.position = "none"),
    get_legend(ggplotTraitMargins),
    nrow = 1, rel_widths = c(3, 1)
  )

ThesisPackage::save_plot(
  gg_plot = FigResultsTraitMargins,
  width = 6, height = 4.5
)
