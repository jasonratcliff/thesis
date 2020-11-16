library(ThesisPackage)
library(cowplot)
library(ggplot2)

# Results: ggplot distribution of specimen Replum Shape.
ggplotTraitReplums <- ThesisPackage::trait_replums %>%
  map_trait_distribution(tidy_trait = .) +
  scale_color_brewer(type = "qual", palette = "Dark2") +
  labs(color = "Replum Shape")

FigResultsTraitReplums <-
  plot_grid(
    ggplotTraitReplums +
      theme(legend.position = "none"),
    get_legend(ggplotTraitReplums),
    nrow = 1, rel_widths = c(2.8, 1.2)
  )

ThesisPackage::save_plot(
  gg_plot = FigResultsTraitReplums,
  width = 6, height = 4.5
)
