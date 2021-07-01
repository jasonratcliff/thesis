library(Thesis)
library(cowplot)
library(ggplot2)

# Results: ggplot distribution of specimen Ovule Number (per locule).
traits <- list()
traits$counts <- Thesis::herbarium_specimens %>%
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

traits$ggplot <- traits$counts %>%
  Thesis::map_trait_distribution(tidy_trait = .) +
  ggplot2::scale_color_viridis_d(option = "D") +
  ggplot2::labs(color = "Ovules per Locule")


traits$map <- traits$ggplot +
  ggplot2::theme(legend.position = "none")

# .pdf ----

traits$pdf_legend <-
  cowplot::get_legend(
    traits$ggplot +
      ggplot2::guides(color = guide_legend(nrow = 1)) +
      ggplot2::theme(legend.title.align = 0.5)
  )

traits$figure_pdf <-
  cowplot::plot_grid(
    traits$map,
    traits$pdf_legend,
    ncol = 1,
    rel_heights = c(0.9, 0.1)
  )

# .png ----

traits$png_legend <-
  cowplot::get_legend(
    traits$ggplot +
      ggplot2::guides(color = guide_legend(ncol = 1)) +
      ggplot2::theme(legend.title.align = 0.5)
  )

traits$figure_png <-
  cowplot::plot_grid(
    traits$map,
    traits$png_legend,
    nrow = 1,
    rel_widths = c(0.8, 0.2)
  )

# cowplot Grid ----

purrr::pwalk(
  .l = list(
    ext = c("png", "pdf"),
    plot = list(traits$figure_png, traits$figure_pdf),
    width = c(5, 6),
    height = c(7, 4),
    aspect = c(.167, 2.5),
    row = c(1, 2),
    col = c(2, 1)
  ),
  .f = function(ext, plot, width, height, aspect, row, col) {
    cowplot::save_plot(
      filename = fs::path("inst/figures/DiscussionTraitOvules", ext = ext),
      plot = plot,
      base_width = width,
      base_height = height,
      base_asp = aspect,
      nrow = row,
      ncol = col
    )
  })

