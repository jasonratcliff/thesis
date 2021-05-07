library(ThesisPackage)
library(cowplot)
library(ggplot2)

# Results: ggplot distribution of specimen Basal Fruit Apices.
ggplotTraitApices <- ThesisPackage::trait_apices %>%
  map_trait_distribution(tidy_trait = .) +
  scale_color_brewer(type = "qual", palette = "Set1") +
  labs(color = "Mature Fruit Apices")

FigDiscussionTraitApices <-
  plot_grid(
    ggplotTraitApices +
      theme(legend.position = "none"),
    get_legend(ggplotTraitApices),
    nrow = 1, rel_widths = c(2.25, 1)
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
      filename = fs::path("inst/figures/DiscussionTraitApices", ext = ext),
      plot = FigDiscussionTraitApices,
      base_width = width,
      base_height = height,
      base_asp = aspect,
      nrow = 1,
      ncol = 1
    )
  })

