library(Thesis)
library(cowplot)
library(ggplot2)

# Results: ggplot distribution of specimen Basal Leaf Margins.
traits <- list()
traits$counts <- Thesis::herbarium_specimens %>%
  Thesis::separate_discrete_trait(
    specimen_tbl = .,
    trait_selection = "Basal_leaf_margins"
  ) %>%
  dplyr::filter(
    !grepl(pattern = paste(c("obtuse", "acute"), collapse = "|"),
           x = .data$Trait)
  ) %>%
  dplyr::mutate(
    Trait = Thesis::capitalize(character_vector = .data$Trait)
  )

traits$ggplot <- traits$counts %>%
  Thesis::map_trait_distribution(tidy_trait = .) +
  ggplot2::scale_color_viridis_d(option = "A") +
  ggplot2::labs(color = "Basal Leaf Margins")

traits$map <- traits$ggplot +
  ggplot2::theme(legend.position = "none")

# .pdf ----

traits$pdf_legend <-
  cowplot::get_legend(
    traits$ggplot +
      ggplot2::guides(color = guide_legend(ncol = 4)) +
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
      filename = fs::path("inst/figures/DiscussionTraitMargins", ext = ext),
      plot = plot,
      base_width = width,
      base_height = height,
      base_asp = aspect,
      nrow = row,
      ncol = col
    )
  })

