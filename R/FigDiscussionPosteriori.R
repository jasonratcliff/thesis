library(ThesisPackage)
library(ggplot2)
library(cowplot)

# Introduction: Build ggplot distribution map with prior annotations.

# Specimen priors total subset
# 
# Bounding box:
# - Longitude: c(-115.2, -103)
# - Latitude: c(37, 49.1)
# 
# Species IDs excluded:
# - Lesquerella
# - cnema
# - alpina
# - cordiformis
# - macrantha
# - geyeri
#
spp_total_posteriori <- ThesisPackage::herbarium_specimens %>%
  dplyr::select("Taxon_a_posteriori", "Latitude", "Longitude") %>%
  subset_coords(specimen_tbl = .,
                Latitude = c(37, 49.1), Longitude = c(-115.2, -103)) %>%

  # Filter out Lesquerella / Physaria sensu lato spp.
  dplyr::filter(
    !grepl(paste("Lesquerella", "cnema", "cordiformis", "macrantha",
                 "rectipes", 
                 sep = "|", collapse = ""),
           x = .data$Taxon_a_posteriori) &
      !grepl("\\?|Brassicaceae", x = Taxon_a_posteriori)
  )

FigDiscussionGgplot <- ggplot() +
  layer_borders(spl_extent = spl_bbox(spp_total_posteriori),
                sf_county_color = "black") +
  layer_specimens(specimen_tbl = spp_total_posteriori, shape_aes = TRUE,
                  id_column = "Taxon_a_posteriori") +
  layer_themes(
    specimen_tbl = spp_total_posteriori,
    id_column = "Taxon_a_posteriori",
    legend_title = "Reviewed Annotations"
  ) +
  coord_sf(
    xlim = range(spl_bbox(spp_total_posteriori)[["Longitude"]]),
    ylim = range(spl_bbox(spp_total_posteriori)[["Latitude"]])
  )

FigDiscussionPosteriori <-
  plot_grid(FigDiscussionGgplot)

ThesisPackage::save_plot(
  gg_plot = FigDiscussionPosteriori,
  height = 5, width = 6
)
