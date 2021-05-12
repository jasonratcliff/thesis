library(Thesis)
library(cowplot)
library(ggplot2)

# Results: ggplot distribution of specimen Fruit Trichomes.
trait_trichomes <- Thesis::herbarium_specimens %>%
  separate_discrete_trait(
    specimen_tbl = .,
    trait_selection = "Fruit_trichomes"
  ) %>%
  dplyr::filter(
    grepl(pattern = paste(c("spreading", "appressed"), collapse = "|"),
          x = .data$Trait)
  ) %>%
  dplyr::mutate(
    Trait = dplyr::case_when(
      grepl("spreading", x = .data$Trait) ~ "Spreading",
      grepl("appressed", x = .data$Trait) ~ "Appressed"
    )
  )

ggplotTraitTrichomes <- trait_trichomes %>%
  map_trait_distribution(tidy_trait = .) +
  scale_color_brewer(palette = "Paired") +
  labs(color = "Fruit Trichomes")

FigDiscussionTraitTrichomes <-
  plot_grid(
    ggplotTraitTrichomes +
      theme(legend.position = "none"),
    get_legend(ggplotTraitTrichomes),
    nrow = 1, rel_widths = c(3, 1)
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
      filename = fs::path("inst/figures/DiscussionTraitTrichomes", ext = ext),
      plot = FigDiscussionTraitTrichomes,
      base_width = width,
      base_height = height,
      base_asp = aspect,
      nrow = 1,
      ncol = 1
    )
  })

