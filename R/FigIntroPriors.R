library(ThesisPackage)
library(ggplot2)

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
#
spp_total_priors <- ThesisPackage::herbarium_specimens %>%
  dplyr::select("prior_id", "Latitude", "Longitude") %>%
  subset_coords(specimen_tbl = .,
                Latitude = c(37, 49.1), Longitude = c(-115.2, -103)) %>%
  # Filter out Lesquerella / Physaria sensu lato spp.
  dplyr::filter(!grepl(paste("Lesquerella", "cnema", "alpina", "cordiformis",
                             "macrantha", sep = "|", collapse = ""),
                       x = prior_id) & !grepl("\\?|Brassicaceae", x = prior_id))

FigIntroPriors <- ggplot() +
  layer_borders(spl_extent = spl_bbox(spp_total_priors),
                sf_county_color = "black") +
  layer_specimens(specimen_tbl = spp_total_priors, shape_aes = TRUE,
                  id_column = "prior_id") +
  layer_themes(specimen_tbl = spp_total_priors, id_column = "prior_id",
               legend_title = "Prior Annotations") +
  coord_sf(xlim = range(spl_bbox(spp_total_priors)[["Longitude"]]),
           ylim = range(spl_bbox(spp_total_priors)[["Latitude"]]))

ThesisPackage::save_plot(gg_plot = FigIntroPriors)
