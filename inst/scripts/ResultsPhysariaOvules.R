library(thesis)
library(dplyr)
library(ggplot2)
library(ggtext)
library(cowplot)

set.seed(20210320)

# Specimens ----
specimens <- list()

# Set of reviewed annotations for species of interest.
specimens$filtered <- thesis::herbarium_specimens %>%
  thesis::filter_reviewed(specimen_tbl = .) %>%
  dplyr::filter(
    !is.na(Ovule_number),
    !is.na(Taxon_a_posteriori),
    !grepl("^Physaria$", .data$Taxon_a_posteriori)
  ) %>%
  dplyr::bind_cols(
    thesis::range_split(trait_tbl = ., split_var = "Ovule_number")
  )

# Ovules ----

traits <- list()

traits$labels <-
  thesis::spl_labels(
    specimen_tbl = specimens$filtered,
    id_column = "Taxon_a_posteriori"
  ) %>%
  gsub("Physaria", "P.", x = .)

# Extract ggplot legend for cowplot grid.
traits$legend <-
  thesis::annotation_legend(
    specimen_tbl = specimens$filtered,
    aesthetic_id = "Taxon_a_posteriori",
    legend_title = "Reviewed Annotations",
    ncol = 6
  )

traits$ovules <-
  thesis::jitter_violin(
    specimen_tbl = specimens$filtered,
    trait = "Ovule_number_max",
    aesthetic_id = "Taxon_a_posteriori",
    violin.params = list(scale = "width"),  # Scale geom_violin() to equal widths
    jitter.params = list(width = 0.25, height = 0.25),  # Jitter positions
    theme.params = list(
      axis.text.x = ggtext::element_markdown(angle = 45, hjust = 1)
    )
  ) +
  ggplot2::scale_x_discrete(labels = traits$labels) +
  ggplot2::labs(y = "Ovules per Locule")

# Physaria saximontana prior annotations
specimens$saximontana <- specimens$filtered %>%
  dplyr::filter(grepl("saximontana", .data$prior_id))

traits$labels_prior <-
  thesis::spl_labels(
    specimen_tbl = specimens$filtered,
    id_column = "prior_id"
  ) %>%
  gsub("Physaria", "P.", x = .)

traits$ovules_prior <-
  ggplot2::ggplot(
    data = specimens$saximontana,
    mapping = aes(x = prior_id, y = Ovule_number_max)
  ) +
  ggplot2::geom_violin(scale = "count", na.rm = TRUE) +
  ggplot2::geom_jitter(
    mapping = aes(color = prior_id, shape = prior_id),
    height = 0.1, width = 0.1, na.rm = TRUE
  ) +
  ggplot2::scale_x_discrete(labels = traits$labels_prior) +
  ggplot2::scale_color_manual(
    name = "Prior Annotations",
    labels = traits$labels_prior,
    values = thesis::spp_color,
    na.value = "black"
  ) +
  ggplot2::scale_shape_manual(
    name = "Prior Annotations",
    labels = traits$labels_prior,
    values = thesis::spp_shape,
    na.value = 17
  ) +
  ggplot2::theme_classic() +
  ggplot2::theme(
    axis.title.x = ggplot2::element_blank(),
    axis.title.y = ggplot2::element_blank(),
    axis.text.x = ggtext::element_markdown(angle = 45, hjust = 1),
    legend.text = ggtext::element_markdown()
  ) +
  ggplot2::facet_wrap(facets = ~ State)

# Grid Plot ----

grids <- list()

grids$top <-
  cowplot::plot_grid(
    cowplot::plot_grid(traits$ovules, labels = "A", vjust = 1),
    cowplot::plot_grid(traits$ovules_prior, labels = "B", hjust = 1, vjust = 1),
    nrow = 1,
    rel_widths = c(2, 1)
  )

grids$bottom <-
  cowplot::plot_grid(traits$legend)

grids$figure <-
  cowplot::plot_grid(
    grids$top, NULL, grids$bottom,
    ncol = 1, rel_heights = c(1, -0.025, 0.25)
  )

purrr::pwalk(
  .l = list(
    ext = c("png", "pdf"),
    width = c(6, 12),
    height = c(8, 3),
    aspect = c(.167, 1.67),
    row = c(1, 2),
    col = c(2, 1)
  ),
  .f = function(ext, width, height, aspect, row, col) {
    cowplot::save_plot(
      filename = fs::path("inst/figures/ResultsPhysariaOvules", ext = ext),
      plot = grids$figure,
      base_width = width,
      base_height = height,
      base_asp = aspect,
      nrow = row,
      ncol = col
    )
  })
