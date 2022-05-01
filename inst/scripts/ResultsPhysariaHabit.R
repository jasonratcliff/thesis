library(thesis)
library(dplyr)
library(fs)
library(ggplot2)
library(ggtext)
library(ggExtra)
library(cowplot)

set.seed(20210320)

# Specimens ----
specimens <- list()

# Set of reviewed annotations for species of interest.
specimens$filtered <- thesis::herbarium_specimens %>%
  thesis::filter_reviewed(specimen_tbl = .) %>%
  dplyr::filter(
    !is.na(Stem_length_dm),
    !is.na(Basal_leaf_length_cm),
    !is.na(Taxon_a_posteriori),
    !grepl("^Physaria$", .data$Taxon_a_posteriori)
  ) %>%
  dplyr::bind_cols(
    thesis::range_split(trait_tbl = ., split_var = "Stem_length_dm"),
    thesis::range_split(trait_tbl = ., split_var = "Basal_leaf_length_cm")
  ) %>%
  dplyr::mutate(rosette_ratio = Stem_length_dm_max * 10 - Basal_leaf_length_cm_max)

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
    ncol = 3
  )

# Basal leaf lengths
traits$leaves <-
  thesis::jitter_violin(
    specimen_tbl = specimens$filtered,
    trait = "Basal_leaf_length_cm_max",
    aesthetic_id = "Taxon_a_posteriori",
    violin.params = list(scale = "width"),
    jitter.params = list(width = 0.25, height = 0),
    theme.params = list(
      axis.text.x = ggplot2::element_blank()
    )
  ) +
  ggplot2::labs(y = "Basal Leaf Length (cm)")

traits$leaves <-
  ggExtra::ggMarginal(
    p = traits$leaves,
    margins = "y",
    type = "violin"
  )

# Stem lengths
traits$stems <-
  thesis::jitter_violin(
    specimen_tbl = specimens$filtered,
    trait = "Stem_length_dm_max",
    aesthetic_id = "Taxon_a_posteriori",
    violin.params = list(scale = "width"),
    jitter.params = list(width = 0.25, height = 0),
    theme.params = list(
      axis.text.x = ggplot2::element_blank()
    )
  ) +
  ggplot2::labs(y = "Stem Length (dm)")

traits$stems <-
  ggExtra::ggMarginal(
    p = traits$stems,
    margins = "y",
    type = "violin"
  )

# Inflorence extension lengths
traits$rosette <-
  thesis::jitter_violin(
    specimen_tbl = specimens$filtered,
    trait = "rosette_ratio",
    aesthetic_id = "Taxon_a_posteriori",
    violin.params = list(scale = "width"),
    jitter.params = list(width = 0.25, height = 0),
    theme.params = list(
      axis.text.x = ggtext::element_markdown(angle = 45, hjust = 1),
      axis.title.x = ggplot2::element_blank()
    )
  ) +
  ggplot2::scale_x_discrete(labels = traits$labels) +
  ggplot2::labs(y = "Inflorescence (cm)")

traits$rosette <-
  ggExtra::ggMarginal(
    p = traits$rosette,
    margins = "y",
    type = "violin"
  )

# Grid Plot ----

grids <- list()

grids$top <-
  cowplot::align_plots(
    traits$leaves,
    traits$stems,
    traits$rosette,
    align = "v",
    axis = "bl"
  )

FigResultsPhysariaHabit <-
  cowplot::plot_grid(
    grids$top[[1]],
    grids$top[[2]],
    grids$top[[3]],
    ncol = 1,
    rel_heights = c(1, 1, 1.4),
    labels = c("A", "B", "C"),
    label_y = 1
  )

purrr::walk(
  .x = c("png", "pdf"),
  .f = function(ext) {
    cowplot::save_plot(
      filename = fs::path("inst/figures/ResultsPhysariaHabit", ext = ext),
      plot = FigResultsPhysariaHabit,
      base_width = 12,
      base_height = 4,
      nrow = 2,
      ncol = 1
    )
  })
