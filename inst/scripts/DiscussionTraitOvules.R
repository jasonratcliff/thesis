library(ThesisPackage)
library(cowplot)
library(ggplot2)

# Results: ggplot distribution of specimen Ovule Number (per locule).
ggplotTraitOvules <- ThesisPackage::trait_ovules %>%
  map_trait_distribution(tidy_trait = .) +
  scale_color_viridis_d(option = "D") +
  labs(color = "Ovules per Locule")

FigDiscussionTraitOvules <-
  plot_grid(
    ggplotTraitOvules +
      theme(legend.position = "none"),
    get_legend(ggplotTraitOvules),
    nrow = 1, rel_widths = c(3, 1)
  )

purrr::pwalk(
  .l = list(
    ext = c("png", "pdf"),
    width = c(10, 5),
    height = c(8, 4.5),
    aspect = c(.167, .167)
  ),
  .f = function(ext, plot, width, height, aspect, row, col) {
    cowplot::save_plot(
      filename = fs::path("inst/figures/DiscussionTraitOvules", ext = ext),
      plot = FigDiscussionTraitOvules,
      base_width = width,
      base_height = height,
      base_asp = aspect,
      nrow = 1,
      ncol = 1
    )
  })

