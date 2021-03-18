library(ThesisPackage)
library(ggplot2)
library(cowplot)

set.seed(20210316)

# Specimens ----

spp_didymocarpa <- herbarium_specimens %>%
  
  subset_coords(
    specimen_tbl = .,
    Longitude = c(-115, 107),
    Latitude = c(42, 49)
    ) %>%
  
  dplyr::filter(
    !(Taxon_a_posteriori %in% paste(
      "Physaria", c("acutifolia", "brassicoides",
                    "condensata", "dornii", "integrifolia")
    )) &
      # Remove specimens with one-off prior identifications.
      !grepl("geyeri|macrantha|cordiformis|nelsonii", .data$prior_id)
  ) %>%
  
  dplyr::select(
    "prior_id", "Taxon_a_posteriori",
    "State", "County", "Latitude", "Longitude",
    "Ovule_number", "Stem_length_dm",
    "Basal_leaf_length_cm", 'Mature_fruit_length_mm'
  ) %>%
  
  # Add reviewed annotation markdown italicization.
  dplyr::mutate(
    Taxon_a_posteriori = .data$Taxon_a_posteriori %>%
      purrr::map_chr(.x = ., function(taxon) {
        gsub("Physaria didymocarpa", "P. d.", taxon) %>% paste0("*", ., "*")
      })
  ) %>%
  
  # Calculate Stem-to-Rosette Ratio
  dplyr::bind_cols(.,
    range_split(trait_tbl = ., split_var = "Stem_length_dm"),
    range_split(trait_tbl = ., split_var = "Basal_leaf_length_cm"),
    range_split(trait_tbl = ., split_var = "Mature_fruit_length_mm"),
    range_split(trait_tbl = ., split_var = "Ovule_number")
  ) %>%
  dplyr::mutate(
    rosette_ratio = Stem_length_dm_max * 10 - Basal_leaf_length_cm_max
  ) %>%
  
  # Sort by uncommon identifications for overplotting.
  dplyr::group_by(.data$Taxon_a_posteriori, .data$prior_id) %>%
  dplyr::add_count() %>%
  dplyr::arrange(dplyr::desc(.data$n)) %>%
  dplyr::ungroup()

# Prior ID Legend ----

# Text markdown label replacements to legend.
trait_labels <- 
  spl_labels(
    specimen_tbl = spp_didymocarpa,
    id_column = "prior_id"
  )

trait_identifications <- ggplot(data = spp_didymocarpa) +
  geom_jitter(
    aes(x = Taxon_a_posteriori, y = prior_id,
        color = prior_id, shape = prior_id), width = 0.1
  ) +
  scale_color_manual(
    name = "Prior Identifications", labels = trait_labels,
    values = ThesisPackage::spp_color, na.value = "black"
  ) +
  scale_shape_manual(
    name = "Prior Identifications", labels = trait_labels,
    values = ThesisPackage::spp_shape, na.value = 17
  ) +
  guides(col = guide_legend(ncol = 3)) +
  theme_classic() +
  theme(
    legend.position = "left",
    legend.title.align = 0,
    legend.text = ggtext::element_markdown()
    )

# Extract ggplot legend for CowPlot grid.
trait_legend <- cowplot::get_legend(trait_identifications)

# Trait ggplots ----

traits <- list()  # Initialize list for trait ggplots

# Ovule count ggplot
traits$ovules <-
  jitter_violin(
    specimen_tbl = spp_didymocarpa,
    trait = "Ovule_number_max",
    violin.params = list(scale = "width"),  # Scale geom_violin() to equal widths 
    jitter.params = list(width = 0.25, height = 0.25),  # Jitter positions
    theme.params = list(axis.text.x = element_blank())
  ) +
    labs(y = "Ovules per Locule")

# Rosette ratio ggplot
traits$rosette <-
  jitter_violin(
    specimen_tbl = spp_didymocarpa,
    trait = "rosette_ratio",
    violin.params = list(scale = "width"),  # Scale geom_violin() to equal widths 
    jitter.params = list(width = 0.25, height = 0),  # Jitter positions
    theme.params = list(axis.text.x = element_blank())
  ) +
  labs(y = "Inflorescence (cm)")

# Max stem length ggplot
traits$stems <-
  jitter_violin(
    specimen_tbl = spp_didymocarpa,
    trait = "Stem_length_dm_max",
    violin.params = list(scale = "width"),  # Scale geom_violin() to equal widths 
    jitter.params = list(width = 0.25, height = 0),  # Jitter positions
    theme.params = list(
      axis.text.x = ggtext::element_markdown(angle = 60, hjust = 1)
    )
  ) +
  labs(y = "Stem Length (dm)")

# Max Basal Leaf Length ggplot
traits$leaves <-
  jitter_violin(
    specimen_tbl = spp_didymocarpa,
    trait = "Basal_leaf_length_cm_max",
    violin.params = list(scale = "width"),  # Scale geom_violin() to equal widths 
    jitter.params = list(width = 0.25, height = 0),  # Jitter positions
    theme.params = list(
      axis.text.x = ggtext::element_markdown(angle = 60, hjust = 1)
    )
  ) +
  labs(y = "Basal Leaf Length (cm)")

# cowplot Grid ----

# Create grobs for plot alignments.
grid_left <- 
  align_plots(traits$ovules, traits$stems, align = "hv", axis = "lbrt")
grid_right <-
  align_plots(traits$rosette, traits$leaves, align = "hv", axis = "lbrt")

# Build plot sections from aligned grobs with labels.
grid_top <-
  plot_grid(grid_left[[1]], grid_right[[1]], nrow = 1,
            labels = c("A", "B"), vjust = -0.5)
grid_bottom <-
  plot_grid(grid_left[[2]], grid_right[[2]], nrow = 1,
            labels = c("C", "D"), vjust = -0.5)

# Build grid from plot sections with legend at bottom.
FigResultsPdidymocarpa <-
  plot_grid(NULL, grid_top, NULL, grid_bottom, # NULL,
            nrow = 4, rel_heights = c(0.1, 1, -0.25, 1)) ##, 0.5))

FigResultsPdidymocarpa <-  # Add x-axis annotation
  ggdraw(add_sub(FigResultsPdidymocarpa, "Reviewed Annotations"))

FigResultsPdidymocarpa <-  # Include legend below plot.
  plot_grid(FigResultsPdidymocarpa, trait_legend, align = "l",
            nrow = 2, rel_heights = c(3, 1))

ThesisPackage::save_plot(
  gg_plot = FigResultsPdidymocarpa,
  height = 7, width = 6
)
