library(ThesisPackage)
library(ggplot2)
options(tigris_use_cache = TRUE)

# Intro Distributions ----

# Figure 1 ----
# - Introduction: Build ggplot map of subet by prior annotations.
Fig01 <- ggplot() +
  layer_borders(spl_extent = spl_bbox(spp_total_priors),
                sf_county_color = "black") +
  layer_specimens(specimen_tbl = spp_total_priors, shape_aes = TRUE,
                  id_column = "prior_id") +
  layer_themes(specimen_tbl = spp_total_priors, id_column = "prior_id",
               legend_title = "Prior Annotations") +
  coord_sf(xlim = range(spl_bbox(spp_total_priors)[["Longitude"]]),
           ylim = range(spl_bbox(spp_total_priors)[["Latitude"]]))

ThesisPackage::save_plot(gg_plot = Fig01)
