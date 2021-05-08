library(ThesisPackage)
library(cowplot)
library(ggplot2)

# Results: ggplot distribution of specimen Basal Leaf Margins.
ggplotTraitMargins <- ThesisPackage::trait_margins %>%
  map_trait_distribution(tidy_trait = .) +
  scale_color_viridis_d(option = "A") +
  labs(color = "Basal Leaf Margins")

FigDiscussionTraitMargins <-
  plot_grid(
    ggplotTraitMargins +
      theme(legend.position = "none"),
    get_legend(ggplotTraitMargins),
    nrow = 1, rel_widths = c(2.5, 1)
  )

purrr::pwalk(
  .l = list(
    ext = c("png", "pdf"),
    width = c(10, 5),
    height = c(8, 5),
    aspect = c(.167, .167)
  ),
  .f = function(ext, plot, width, height, aspect, row, col) {
    cowplot::save_plot(
      filename = fs::path("inst/figures/DiscussionTraitMargins", ext = ext),
      plot = FigDiscussionTraitMargins,
      base_width = width,
      base_height = height,
      base_asp = aspect,
      nrow = 1,
      ncol = 1
    )
  })

