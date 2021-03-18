library(ThesisPackage)
library(ggplot2)
library(cowplot)

set.seed(20210316)

# Specimens ----

spp_integrifolia <- ThesisPackage::herbarium_specimens %>%
  subset_coords(specimen_tbl = .,
                Latitude = c(41, 44), Longitude = c(-111.7, -107.7)) %>%
  dplyr::filter(
    .data$State == "Wyoming" & .data$County %in%
      c("Uinta", "Lincoln", "Sublette", "Teton", "Sweetwater") |
      .data$State == "Idaho" & .data$Longitude > -112,
    !is.na(.data$Latitude) & !is.na(.data$Longitude),
    !grepl(pattern = "^Physaria$", x = .data$Taxon_a_posteriori)
  ) %>%
  dplyr::select("prior_id", "Taxon_a_posteriori",
                "State", "County", "Latitude", "Longitude",
                "Stem_length_dm", "Basal_leaf_length_cm", "Ovule_number") %>%

  # Calculate Stem-to-Rosette Ratio
  dplyr::bind_cols(.,
    range_split(trait_tbl = ., split_var = "Stem_length_dm"),
    range_split(trait_tbl = ., split_var = "Basal_leaf_length_cm"),
    range_split(trait_tbl = ., split_var = "Ovule_number")
  ) %>%
  dplyr::mutate(
    rosette_ratio = Stem_length_dm_max * 10 - Basal_leaf_length_cm_max,
    
    # Add reviewed annotation markdown italicization.
    Taxon_a_posteriori = .data$Taxon_a_posteriori %>%
      purrr::map_chr(.x = ., function(taxon) {
        gsub("Physaria", "P.", taxon) %>% paste0("*", ., "*")
      })
  ) %>%

  # Sort by uncommon identifications for overplotting.
  dplyr::group_by(.data$Taxon_a_posteriori, .data$prior_id) %>%
  dplyr::add_count() %>%
  dplyr::arrange(dplyr::desc(.data$n)) %>%
  dplyr::ungroup()

# Prior ID Legend ----

# Text markdown label replacements to legend.
trait_labels <- 
  spl_labels(specimen_tbl = spp_integrifolia, id_column = "prior_id")

trait_identifications <- ggplot(data = spp_integrifolia) +
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
    specimen_tbl = spp_integrifolia,
    trait = "Ovule_number_max",
    violin.params = list(scale = "width"),  # Scale geom_violin() to equal widths
    jitter.params = list(width = 0.25, height = 0.25),  # Jitter positions
    theme.params = list(axis.text.x = element_blank())
  ) +
  labs(y = "Ovules per Locule")

# Rosette ratio ggplot
traits$rosette <-
  jitter_violin(
    specimen_tbl = spp_integrifolia,
    trait = "rosette_ratio",
    violin.params = list(scale = "width"),  # Scale geom_violin() to equal widths 
    jitter.params = list(width = 0.25, height = 0),  # Jitter positions
    theme.params = list(axis.text.x = element_blank())
  ) +
  labs(y = "Inflorescence (cm)")

# Max stem length ggplot
traits$stems <-
  jitter_violin(
    specimen_tbl = spp_integrifolia,
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
    specimen_tbl = spp_integrifolia,
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
FigResultsPintegrifolia <-
  plot_grid(NULL, grid_top, NULL, grid_bottom,
            nrow = 4, rel_heights = c(0.1, 1, -0.175, 1))

FigResultsPintegrifolia <-  # Add x-axis annotation
  ggdraw(add_sub(FigResultsPintegrifolia, "Reviewed Annotations"))
FigResultsPintegrifolia <-
  plot_grid(FigResultsPintegrifolia, trait_legend,
            nrow = 2, rel_heights = c(2, 0.5))

ThesisPackage::save_plot(
  gg_plot = FigResultsPintegrifolia,
  height = 6, width = 6
)
