library(ThesisPackage)
library(cowplot)
library(ggplot2)

# Results: ggplot distribution of specimen Basal Fruit Apices.
ggplotTraitApices <- ThesisPackage::trait_apices %>%
  map_trait_distribution(tidy_trait = .) +
  scale_color_brewer(type = "qual", palette = "Set1") +
  labs(color = "Mature Fruit Apices")

FigResultsTraitApices <-
  plot_grid(
    ggplotTraitApices +
      theme(legend.position = "none"),
    get_legend(ggplotTraitApices),
    nrow = 1, rel_widths = c(2.8, 1.2)
  )

ThesisPackage::save_plot(
  gg_plot = FigResultsTraitApices,
  width = 6, height = 4.5
)
