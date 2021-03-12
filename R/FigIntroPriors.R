library(ThesisPackage)
library(dplyr)
library(ggplot2)
library(cowplot)

set.seed(20210311)

# Introduction: Build ggplot distribution map with prior annotations.
spp_total_priors <- ThesisPackage::herbarium_specimens %>%
  select("prior_id", "Latitude", "Longitude") %>%
  filter(prior_id %in%
    paste("Physaria",
      c("acutifolia", "brassicoides", "condensata", "dornii",
        "eburniflora", "integrifolia", "vitulifera",
        "medicinae", "chambersii", "rollinsii",
        paste("didymocarpa ssp.", c("didymocarpa", "lanata", "lyrata")),
        paste("saximontana ssp.", c("saximontana", "dentata")),
        paste("floribunda ssp.", c("floribunda", "osterhoutii"))
      )
    ) | grepl("^Physaria$", .data$prior_id)
  ) %>%
  subset_coords(
    specimen_tbl = .,
    Latitude = c(37, 49.1),
    Longitude = c(-114.5, -102)
  )

FigIntroPriors <- ggplot() +
  layer_borders(
    spl_extent = spl_bbox(spp_total_priors),
    sf_county_color = "black"
  ) +
  layer_specimens(
    specimen_tbl = spp_total_priors,
    shape_aes = TRUE,
    id_column = "prior_id",
    jitter_width = 0.1,
    jitter_height = 0.1,
    jitter_alpha = 0.75
  ) +
  layer_themes(
    specimen_tbl = spp_total_priors,
    id_column = "prior_id",
    legend_title = "Prior Annotations"
  ) +
  coord_sf(
    xlim = range(spl_bbox(spp_total_priors)[["Longitude"]]),
    ylim = range(spl_bbox(spp_total_priors)[["Latitude"]])
  )

FigIntroPriors <- plot_grid(FigIntroPriors)

ThesisPackage::save_plot(
  gg_plot = FigIntroPriors,
  height = 5, width = 6
)
