library(Thesis)
library(cowplot)
library(dplyr)
library(fs)
library(ggExtra)
library(ggplot2)
library(ggridges)
library(ggtext)
library(grid)
library(gridExtra)
library(gtable)


set.seed(20210320)

# Specimens ----
specimens <- list()

# Set of reviewed annotations for species of interest.
specimens$filtered <- Thesis::herbarium_specimens %>%
  Thesis::filter_reviewed(specimen_tbl = .) %>%
  dplyr::filter(
    !is.na(Elev_raw_max),
    !is.na(Taxon_a_posteriori),
    !grepl("^Physaria$", .data$Taxon_a_posteriori)
  )

specimens$median <- specimens$filtered %>%
  dplyr::filter(!is.na(Taxon_a_posteriori)) %>%
  dplyr::select(Taxon_a_posteriori, Elev_raw_max) %>%
  dplyr::group_by(Taxon_a_posteriori) %>%
  dplyr::summarize(
    Median = median(Elev_raw_max, na.rm = TRUE) %>% round(),
    MAD = mad(Elev_raw_max, na.rm = TRUE) %>% round(),
    n = n()
  ) %>%
  dplyr::filter(!is.nan(Median)) %>%
  dplyr::arrange(Median) %>%
  dplyr::rename("Species" = .data$Taxon_a_posteriori)

# Elevation Violin ----

traits <- list()

traits$labels <-
  Thesis::spl_labels(
    specimen_tbl = specimens$filtered,
    id_column = "Taxon_a_posteriori"
  ) %>%
  gsub("Physaria", "P.", x = .)

# Extract ggplot legend for cowplot grid.
traits$legend <-
  Thesis::annotation_legend(
    specimen_tbl = specimens$filtered,
    aesthetic_id = "Taxon_a_posteriori",
    legend_title = "Reviewed Annotations",
    ncol = 1
  )

traits$elevation <-
  Thesis::jitter_violin(
    specimen_tbl = specimens$filtered,
    trait = "Elev_raw_max",
    aesthetic_id = "Taxon_a_posteriori",
    aesthetic_labels = traits$labels,
    violin.params = list(scale = "width"),
    jitter.params = list(size = 3, height = 0),
    theme.params = list(
      axis.text.x = ggtext::element_markdown(angle = 45, hjust = 1)
    )
  ) +
  ggplot2::scale_x_discrete(
    limits = specimens$median$Species,
    labels = traits$labels
  ) +
  ggplot2::labs(y = "Maximum Elevation (ft.)")

traits$elevation <-
  ggExtra::ggMarginal(
    p = traits$elevation,
    margins = "y",
    type = "violin"
  )

# Elevation Ridgeline ----

traits$ridgeline <- ggplot2::ggplot(data = specimens$filtered) +
  ggridges::geom_density_ridges(
    mapping = ggplot2::aes(
      x = Elev_raw_max,
      y = Taxon_a_posteriori,
      fill = Taxon_a_posteriori
    ),
    na.rm = TRUE,
    bandwidth = 500
  ) +
  ggplot2::theme_classic() +
  ggplot2::scale_x_continuous(name = "Maximum Elevation (ft.)") +
  ggplot2::scale_y_discrete(
    labels = traits$labels,
    limits = rev(specimens$median$Species)) +
  ggplot2::scale_fill_manual(
    name = "Reviewed Annotation",
    values = Thesis::spp_color,
    labels = desc(traits$labels)
  ) +
  ggplot2::theme(
    legend.position = "none",
    legend.text = ggtext::element_markdown(),
    axis.text.y = ggtext::element_markdown(),
    axis.title.y = ggplot2::element_blank()
  )

# Table ----

specimens$table <- specimens$Mean %>%
    mutate(  # Italicization
      Species = purrr::map_chr(.x = Species, function(taxon) {
        taxon_split <- unlist(strsplit(taxon, " "))
        if (length(taxon_split) < 3) {
          parsed_label <-
            paste0(
              c("italic(", paste(taxon_split, collapse = " "), ")"),
              collapse = ""
            )
        } else {
          parsed_label <-
            paste0(
              "italic(",
              paste(taxon_split[1:2], collapse = " "),
              ") ssp. italic(",
              taxon_split[4],
              ")",
              collapse = ""
            )
        }
        parsed_label <- unlist(parsed_label) %>%
          gsub(" +", "~", x = .)  # Replace white space to parse by expression()
        return(parsed_label)
      })
    ) %>%
  dplyr::rename_all(~ paste0("bold(", .x, ")"))

traits$grob <-
  gridExtra::tableGrob(
    d = specimens$table, rows = NULL,
    theme = gridExtra::ttheme_default(
      parse = TRUE, # Parse expressions for column names and subspecies.
      core = list(
        fg_params = list(
          # Define matrices for horizontal adjustment and x-axis positioning.
          hjust = matrix(c(0, 0, 0.5, 1), ncol = 4,
                         nrow = nrow(specimens$table), byrow = TRUE),
          x = matrix(c(0.025, 0.1, 0.5, 0.9), ncol = 4,
                     nrow = nrow(specimens$table), byrow = TRUE),
          fontsize = 8
        )
      )
    )
  )

traits$grob <-  # Add column name rectangle
  gtable::gtable_add_grob(
    x = traits$grob,
    grobs = grid::rectGrob(gp = grid::gpar(fill = NA, lwd = 4)),
    t = 1,  # top
    r = ncol(traits$grob),  # right
    b = 1,  # bottom
    l = 1  # left
  )

traits$grob <-  # Add table rectangle
  gtable::gtable_add_grob(
    x = traits$grob,
    grobs = grid::rectGrob(gp = grid::gpar(fill = NA, lwd = 3)),
    t = 2,  # top
    r = ncol(traits$grob),  # right
    b = nrow(traits$grob),  # bottom
    l = 1  # left
  )

# Add column separators for each column in the grob.
for (i in 2:ncol(traits$grob)) {
  traits$grob <- 
    gtable::gtable_add_grob(
      x = traits$grob,
      grobs = grid::rectGrob(gp = grid::gpar(fill = NA, lwd = 3)),
      t = 1,  # top
      r = ncol(traits$grob),  # right
      b = nrow(traits$grob),  # bottom
      l = i  # left
    )
}

# Grid Plot ----

grids <- list()

grids$bottom_right <-
  cowplot::plot_grid(
    traits$grob,
    labels = "C",
    label_x = 0.05,
    label_y = 1
  )

grids$bottom <-
  cowplot::plot_grid(
    traits$ridgeline,
    grids$bottom_right,
    rel_widths = c(1, 1),
    align = "h",
    axis = "tb",
    labels = "B",
    label_x = 0.175,
    label_y = 1.0075
  )

grids$align <-
  cowplot::align_plots(
    traits$elevation,
    traits$ridgeline,
    align = "v",
    axis = "l"
  )

grids$top <-
  cowplot::plot_grid(
    grids$align[[1]],
    labels = "A",
    label_x = 0.09,
    label_y = 1
  )

FigResultsPhysariaElevation <-
  cowplot::plot_grid(
    grids$top,
    grids$bottom,
    ncol = 1
  )

purrr::walk(
  .x = c("png", "pdf"),
  .f = function(ext) {
    cowplot::save_plot(
      filename = fs::path("inst/figures/ResultsPhysariaElevation", ext = ext),
      plot = FigResultsPhysariaElevation,
      base_width = 12,
      base_height = 4,
      nrow = 2,
      ncol = 1
    )
  })

