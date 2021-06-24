library(Thesis)
library(dplyr)
library(ggplot2)
library(ggtext)
library(ggExtra)
library(cowplot)

set.seed(20210319)

# Specimens ----
specimens <- list()

# Set of reviewed annotations for species of interest.
specimens$filtered <- Thesis::herbarium_specimens %>%
  Thesis::filter_reviewed(specimen_tbl = .) %>%
  dplyr::filter(
    !is.na(Mature_fruit_length_mm),
    !is.na(Taxon_a_posteriori),
    !grepl("^Physaria$", .data$Taxon_a_posteriori)
  ) %>%
  dplyr::bind_cols(
    Thesis::range_split(trait_tbl = ., split_var = "Mature_fruit_length_mm")
  )

# Fruits ----

traits <- list()

traits$labels <-
  Thesis::spl_labels(
    specimen_tbl = specimens$filtered,
    id_column = "Taxon_a_posteriori"
  ) %>%
  gsub("Physaria", "P.", x = .)

# Extract ggplot legend for cowplot grid.
traits$legend_pdf <-
  Thesis::annotation_legend(
    specimen_tbl = specimens$filtered,
    aesthetic_id = "Taxon_a_posteriori",
    legend_title = "Reviewed Annotations",
    ncol = 1
  )

traits$legend_png <-
  Thesis::annotation_legend(
    specimen_tbl = specimens$filtered,
    aesthetic_id = "Taxon_a_posteriori",
    legend_title = "Reviewed Annotations",
    ncol = 6
  )

traits$fruits <-
  Thesis::jitter_violin(
    specimen_tbl = specimens$filtered,
    trait = "Mature_fruit_length_mm_max",
    aesthetic_id = "Taxon_a_posteriori",
    aesthetic_labels = traits$labels,
    legend_title = "Reviewed Annotations",
    violin.params = list(scale = "width"),
    jitter.params = list(width = 0.25),
    theme.params = list(
      axis.text.x = ggtext::element_markdown(angle = 45, hjust = 1),
      axis.title.y = ggplot2::element_text(size = 10)
    )
  ) +
  ggplot2::scale_x_discrete(labels = traits$labels) +
  ggplot2::labs(y = "Fruit Length (mm)")

traits$fruits <-
  ggExtra::ggMarginal(
    p = traits$fruits,
    margins = "y",
    type = "violin"
  )

traits$phenology <-
  Thesis::trait_phenology(
    specimen_tbl = specimens$filtered,
    trait = "Mature_fruit_length_mm_max",
    aesthetic_id = "Taxon_a_posteriori",
    aesthetic_labels = traits$labels,
    legend_title = "Reviewed Annotations",
    theme.params = list(
      legend.position = "none",
      strip.text = ggtext::element_markdown(size = 7),
      axis.text.x = ggplot2::element_text(size = 6),
      axis.title.y = ggplot2::element_text(size = 10)
    )
  ) +
  ggplot2::scale_y_continuous(limits = c(0, 20)) +
  ggplot2::facet_wrap(
    facets = ~ Taxon_a_posteriori, ncol = 3,
    labeller = ggplot2::labeller(Taxon_a_posteriori = traits$labels)
  ) +
  ggplot2::labs(y = "Fruit Length (mm)", x = "Collection Date")

# Grid Plot ----

traits$png_fruit <-
  cowplot::plot_grid(
    traits$fruits,
    traits$legend_png,
    ncol = 1,
    rel_heights = c(1, 0.25)
  )

traits$png_phenology <-
  cowplot::plot_grid(
    traits$phenology,
    traits$legend_pdf,
    nrow = 1, rel_widths = c(2, 1)
  )

traits$fruit_pdf <-
  cowplot::plot_grid(
    traits$fruits,
    cowplot::plot_grid(
      traits$phenology,
      traits$legend_pdf,
      nrow = 1, rel_widths = c(2, 1)
    ),
    ncol = 1, rel_heights = c(1, 2),
    labels = c("A", "B")
  )

purrr::pwalk(
  .l = list(
    path = c(
      "ResultsPhysariaFruits",
      "ResultsPhysariaFruitsPhenology",
      "ResultsPhysariaFruits"
    ),
    ext = c("png", "png", "pdf"),
    plot = list(traits$png_fruit, traits$png_phenology, traits$fruit_pdf),
    width = c(6, 6, 6),
    height = c(8, 8, 4),
    aspect = c(.167, .167, 2.5),
    row = c(1, 1, 2),
    col = c(2, 2, 1)
  ),
  .f = function(path, ext, plot, width, height, aspect, row, col) {
    cowplot::save_plot(
      filename = fs::path("inst/figures", path, ext = ext),
      plot = plot,
      base_width = width,
      base_height = height,
      base_asp = aspect,
      nrow = row,
      ncol = col
    )
  })

