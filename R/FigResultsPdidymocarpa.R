library(ThesisPackage)
library(ggplot2)
library(cowplot)

# Specimens ----

spp_didymocarpa <- herbarium_specimens %>%
  dplyr::filter(
    grepl(
      pattern = "Wyoming|Montana|Idaho",
      x = .data$State
    ) &
      (
        grepl(
          pattern = "didymocarpa|saximontana|eburniflora",
          x = .data$prior_id
        ) &
          grepl(
            pattern = "^Physaria$|didymocarpa|saximontana|eburniflora",
            x = .data$Taxon_a_posteriori
          )
      ),
    .data$Latitude > 44 | .data$Longitude > -110
  ) %>%
  dplyr::select(
    "prior_id", "Taxon_a_posteriori",
    "State", "County", "Latitude", "Longitude",
    "Stem_length_dm", "Basal_leaf_length_cm", 'Mature_fruit_length_mm'
  ) %>%
  
  # Calculate Stem-to-Rosette Ratio
  dplyr::bind_cols(.,
    range_split(trait_tbl = ., split_var = "Stem_length_dm"),
    range_split(trait_tbl = ., split_var = "Basal_leaf_length_cm"),
    range_split(trait_tbl = ., split_var = "Mature_fruit_length_mm")
  ) %>%
  dplyr::mutate(
    rosette_ratio = Stem_length_dm_max * 10 - Basal_leaf_length_cm_max
  ) %>%
  
  # Sort by uncommon identifications for overplotting.
  dplyr::group_by(.data$Taxon_a_posteriori, .data$prior_id) %>%
  dplyr::add_count() %>%
  dplyr::arrange(dplyr::desc(.data$n)) %>%
  dplyr::ungroup()

# Mapping ----

# Function wrapper to plot P. didymocarpa prior and reviewed distributions.
map_didymocarpa <- function(specimen_tbl, id_column, legend_title, ...) {
  markdown_labels <-
    spl_labels(specimen_tbl = specimen_tbl, id_column = id_column)
  base_map <- ggplot() +
    layer_borders(
      spl_extent = spl_bbox(specimen_tbl),
      sf_county_color = "black"
    ) +
    layer_specimens(
      specimen_tbl = specimen_tbl,
      id_column = id_column, shape_aes = TRUE
    ) +
    scale_color_manual(
      name = legend_title, labels = markdown_labels,
      values = ThesisPackage::spp_color, na.value = "black"
    ) +
    scale_shape_manual(
      name = legend_title, labels = markdown_labels,
      values = ThesisPackage::spp_shape, na.value = 17
    ) +
    theme(
      panel.background = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(fill = NA, color =  "black"),
      legend.key = ggplot2::element_blank(),
      legend.text = ggtext::element_markdown()
    )
  return(base_map)
}

map_priors <- 
  map_didymocarpa(
    specimen_tbl = spp_didymocarpa,
    id_column = "prior_id",
    legend_title = "Priors"
  ) +
  coord_sf(
    xlim = range(spl_bbox(spp_didymocarpa)[["Longitude"]]),
    ylim = range(spl_bbox(spp_didymocarpa)[["Latitude"]])
  )

map_reviewed <- 
  map_didymocarpa(
    specimen_tbl = spp_didymocarpa,
    id_column = "Taxon_a_posteriori",
    legend_title = "Annotations"
  ) +
  coord_sf(
    xlim = range(spl_bbox(spp_didymocarpa)[["Longitude"]]),
    ylim = range(spl_bbox(spp_didymocarpa)[["Latitude"]])
  )

# Build joint legend with all ID combinations.
spp_legend <- spp_didymocarpa %>%
  dplyr::select(
    "prior_id", "Taxon_a_posteriori",
    "Latitude", "Longitude"
  ) %>%
  tidyr::pivot_longer(
    cols = c("prior_id", "Taxon_a_posteriori"),
    values_to = "Taxon"
    ) %>%
  map_didymocarpa(
    specimen_tbl = .,
    id_column = "Taxon",
    legend_title = "Annotations"
  )

FigResultsPdidymocarpaMap <-
  plot_grid(
    plot_grid(
      map_priors + theme(legend.position = "none"),
      map_reviewed + theme(legend.position = "none"),
      nrow = 1, labels = c("A", "B")
    ),
    get_legend(
      spp_legend +
        theme(legend.title.align = 0.5) + 
        guides(col = guide_legend(ncol = 2))
    ),
    nrow = 2, rel_heights = c(3, 1)
  )

ThesisPackage::save_plot(
  gg_plot = FigResultsPdidymocarpaMap,
  height = 4.75, width = 6
)

# Legend ----

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
  theme_classic() +
  theme(legend.text = ggtext::element_markdown())

# Extract ggplot legend for CowPlot grid.
trait_legend <- cowplot::get_legend(trait_identifications)

# Trait ggplots ----

# Add reviewed annotation markdown italicization.
spp_didymocarpa <- spp_didymocarpa %>%
  dplyr::mutate(
    Taxon_a_posteriori = .data$Taxon_a_posteriori %>%
      purrr::map_chr(.x = ., function(taxon) {
        gsub("Physaria didymocarpa", "P. d.", taxon) %>% paste0("*", ., "*")
      })
  )

# Rosette Ratio ggplot
trait_rosette <- ggplot(data = spp_didymocarpa) +
  geom_violin(
    aes(x = Taxon_a_posteriori, y = rosette_ratio), na.rm = TRUE
  ) +
  scale_color_manual(values = ThesisPackage::spp_color) +
  ggnewscale::new_scale_color() +
  geom_jitter(
    aes(x = Taxon_a_posteriori, y = rosette_ratio,
        color = prior_id, shape = prior_id),
    width = 0.1, height = 0, na.rm = TRUE
  ) +
  scale_color_manual(values = ThesisPackage::spp_color, na.value = "black") +
  scale_shape_manual(values = ThesisPackage::spp_shape, na.value = 17) +
  labs(y = "Rosette-to-Inflorescence (cm)") +
  theme_classic() +
  theme(
    title = ggtext::element_markdown(),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    legend.position = "none"
  )

# Max Stem Length ggplot
trait_stems <- ggplot(data = spp_didymocarpa) +
  geom_violin(
    aes(x = Taxon_a_posteriori, y = Stem_length_dm_max), na.rm = TRUE
  ) +
  geom_jitter(
    aes(x = Taxon_a_posteriori, y = Stem_length_dm_max,
        color = prior_id, shape = prior_id),
    width = 0.1, height = 0, na.rm = TRUE
  ) +
  scale_color_manual(values = ThesisPackage::spp_color, na.value = "black") +
  scale_shape_manual(values = ThesisPackage::spp_shape, na.value = 17) +
  labs(y = "Max Stem Length (dm)") +
  theme_classic() +
  theme(
    axis.text.x = ggtext::element_markdown(angle = 60, hjust = 1),
    axis.title.x = element_blank(),
    legend.position = "none"
  )

# Max Basal Leaf Length ggplot
trait_leaves <- ggplot(data = spp_didymocarpa) +
  geom_violin(
    aes(x = Taxon_a_posteriori, y = Basal_leaf_length_cm_max), na.rm = TRUE
  ) +
  geom_jitter(
    aes(x = Taxon_a_posteriori, y = Basal_leaf_length_cm_max,
        color = prior_id, shape = prior_id),
    width = 0.1, height = 0, na.rm = TRUE
  ) +
  scale_color_manual(values = ThesisPackage::spp_color, na.value = "black") +
  scale_shape_manual(values = ThesisPackage::spp_shape, na.value = 17) +
  labs(y = "Max Basal Leaf Length (cm)") +
  theme_classic() +
  theme(
    axis.text.x = ggtext::element_markdown(angle = 60, hjust = 1),
    axis.title.x = element_blank(),
    legend.position = "none"
  )

# cowplot Grid ----

grid_left <- 
  align_plots(trait_rosette, trait_stems, align = "hv", axis = "lbrt")

grid_top <-
  plot_grid(grid_left[[1]],
    plot_grid(NULL, trait_legend, NULL, ncol = 1, rel_heights = c(0.1, 1, 1)),
    labels = c("A", ""), nrow = 1
  )

grid_bottom <-
  plot_grid(grid_left[[2]], trait_leaves, labels =  c("B", "C"), nrow = 1)

FigResultsPdidymocarpa <-
  plot_grid(grid_top, NULL, grid_bottom, nrow = 3, rel_heights = c(1, -0.3, 1))

FigResultsPdidymocarpa <-
  ggdraw(add_sub(FigResultsPdidymocarpa, "Reviewed Annotations"))

ThesisPackage::save_plot(
  gg_plot = FigResultsPdidymocarpa,
  height = 7, width = 6
)
