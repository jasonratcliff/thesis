library(ThesisPackage)
library(cowplot)
library(ggplot2)

# Results: ggplot distribution of specimen Fruit Trichomes.
ggplotTraitTrichomes <- ThesisPackage::trait_trichomes %>%
  map_trait_distribution(tidy_trait = .) +
  scale_color_brewer(palette = "Paired") +
  labs(color = "Fruit Trichomes")

FigResultsTraitTrichomes <-
  plot_grid(
    ggplotTraitTrichomes +
      theme(legend.position = "none"),
    get_legend(ggplotTraitTrichomes),
    nrow = 1, rel_widths = c(3, 1)
  )

ThesisPackage::save_plot(
  gg_plot = FigResultsTraitTrichomes,
  width = 6, height = 4.5
)
