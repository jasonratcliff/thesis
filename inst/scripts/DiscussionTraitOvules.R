library(Thesis)
library(cowplot)
library(ggplot2)

# Results: ggplot distribution of specimen Ovule Number (per locule).
trait_ovules <- Thesis::herbarium_specimens %>%
  dplyr::select(
    "prior_id", "Taxon_a_posteriori", "Ovule_number",
    "Latitude", "Longitude", "Collector", "Collection_Number"
  ) %>%
  dplyr::filter(!is.na(.data$Ovule_number)) %>%
  dplyr::bind_cols(., range_split(trait_tbl = .,
                                  split_var = "Ovule_number")) %>%
  dplyr::rename(Trait = "Ovule_number_max") %>%
  dplyr::mutate(
    Trait = as.factor(x = .data$Trait)
  )

ggplotTraitOvules <- trait_ovules %>%
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

