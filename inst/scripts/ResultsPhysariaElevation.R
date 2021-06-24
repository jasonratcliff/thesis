library(Thesis)
library(cowplot)
library(dplyr)
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
  filter_reviewed(specimen_tbl = .) %>%
  filter(
    !is.na(Elev_raw_max),
    !is.na(Taxon_a_posteriori),
    !grepl("^Physaria$", .data$Taxon_a_posteriori)
  )

specimens$median <- specimens$filtered %>%
  filter(!is.na(Taxon_a_posteriori)) %>%
  select(Taxon_a_posteriori, Elev_raw_max) %>%
  group_by(Taxon_a_posteriori) %>%
  summarize(
    Median = median(Elev_raw_max, na.rm = TRUE) %>% round(),
    MAD = mad(Elev_raw_max, na.rm = TRUE) %>% round(),
    n = n()
  ) %>%
  filter(!is.nan(Median)) %>%
  arrange(Median) %>%
  rename("Species" = .data$Taxon_a_posteriori)

# Elevation Violin ----

traits <- list()

traits$labels <-
  spl_labels(
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
  jitter_violin(
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
  scale_x_discrete(
    limits = specimens$median$Species,
    labels = traits$labels
  ) +
  labs(y = "Maximum Elevation (ft.)")

traits$elevation <-
  ggMarginal(
    p = traits$elevation,
    margins = "y",
    type = "violin"
  )

# Elevation Ridgeline ----

traits$ridgeline <- ggplot(data = specimens$filtered) +
  geom_density_ridges(
    mapping = aes(
      x = Elev_raw_max,
      y = Taxon_a_posteriori,
      fill = Taxon_a_posteriori
    ),
    na.rm = TRUE,
    bandwidth = 500
  ) +
  theme_classic() +
  scale_x_continuous(name = "Maximum Elevation (ft.)") +
  scale_y_discrete(
    labels = traits$labels,
    limits = rev(specimens$median$Species)) +
  scale_fill_manual(
    name = "Reviewed Annotation",
    values = Thesis::spp_color,
    labels = desc(traits$labels)
  ) +
  theme(
    legend.position = "none",
    legend.text = ggtext::element_markdown(),
    axis.text.y = ggtext::element_markdown(),
    axis.title.y = element_blank()
  )

# Table ----

specimens$table <- specimens$median %>%
    mutate(  # Italicization
      Species = Thesis::parse_taxa(tree_tibble = ., id_column = "Species")
    ) %>%
  rename_all(~ paste0("bold(", .x, ")"))

traits$grob <-
  tableGrob(
    d = specimens$table, rows = NULL,
    theme = ttheme_default(
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
  plot_grid(
    traits$grob,
    labels = "C",
    label_x = 0.05,
    label_y = 1
  )

grids$bottom <-
  plot_grid(
    traits$ridgeline,
    # traits$grob,
    grids$bottom_right,
    rel_widths = c(1, 1),
    align = "h",
    axis = "tb",
    labels = "B",
    label_x = 0.175,
    label_y = 1.0075
  )

grids$align <-
  align_plots(
    traits$elevation,
    traits$ridgeline,
    align = "v",
    axis = "l"
  )

grids$top <-
  plot_grid(
    grids$align[[1]],
    labels = "A",
    label_x = 0.09,
    label_y = 1
  )

FigResultsPhysariaElevation <-
  plot_grid(
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

