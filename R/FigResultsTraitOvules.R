library(ThesisPackage)
library(cowplot)
library(ggplot2)

# Results: ggplot distribution of specimen Ovule Number (per locule).
ggplotTraitOvules <- ThesisPackage::trait_ovules %>%
  map_trait_distribution(tidy_trait = .) +
  scale_color_viridis_d(option = "D") +
  labs(color = "Ovules per Locule")

FigResultsTraitOvules <-
  plot_grid(
    ggplotTraitOvules +
      theme(legend.position = "none"),
    get_legend(ggplotTraitOvules),
    nrow = 1, rel_widths = c(3, 1)
  )

ThesisPackage::save_plot(
  gg_plot = FigResultsTraitOvules,
  width = 6, height = 4.5
)
