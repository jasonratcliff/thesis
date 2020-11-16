library(ThesisPackage)
library(ggplot2)
library(cowplot)

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
                "Stem_length_dm", "Basal_leaf_length_cm") %>%

  # Calculate Stem-to-Rosette Ratio
  dplyr::bind_cols(.,
    range_split(trait_tbl = ., split_var = "Stem_length_dm"),
    range_split(trait_tbl = ., split_var = "Basal_leaf_length_cm")
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

# Legend ----

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
  theme_classic() +
  theme(legend.text = ggtext::element_markdown())

# Extract ggplot legend for CowPlot grid.
trait_legend <- cowplot::get_legend(trait_identifications)

# Trait ggplots ----

# Rosette Ratio ggplot
trait_rosette <- ggplot(data = spp_integrifolia) +
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
  labs(y = "Inflorescence (cm)") +
  theme_classic() +
  theme(
    title = ggtext::element_markdown(),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    legend.position = "none"
  )

# Max Stem Length ggplot
trait_stems <- ggplot(data = spp_integrifolia) +
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
  labs(y = "Stem Length (dm)") +
  theme_classic() +
  theme(
    axis.text.x = ggtext::element_markdown(angle = 60, hjust = 1),
    axis.title.x = element_blank(),
    legend.position = "none"
  )

# Max Basal Leaf Length ggplot
trait_leaves <- ggplot(data = spp_integrifolia) +
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
  labs(y = "Basal Leaf Length (cm)") +
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
    plot_grid(NULL, trait_legend, NULL, ncol = 1, rel_heights = c(0.2, 1, 1)),
    labels = c("A", ""), nrow = 1
  )

grid_bottom <-
  plot_grid(grid_left[[2]], trait_leaves, labels =  c("B", "C"), nrow = 1)

FigResultsPintegrifolia <-
  plot_grid(grid_top, NULL, grid_bottom, nrow = 3, rel_heights = c(1, -0.175, 1))

FigResultsPintegrifolia <-
  ggdraw(add_sub(FigResultsPintegrifolia, "Reviewed Annotations"))

ThesisPackage::save_plot(
  gg_plot = FigResultsPintegrifolia,
  height = 6, width = 6
)
