library(ThesisPackage)
library(dplyr)
library(ggplot2)
library(cowplot)

set.seed(20210311)

# Discussion: Build ggplot distribution map with reviewed annotations.
spp_total_posteriori <- ThesisPackage::herbarium_specimens %>%
  select("Taxon_a_posteriori", "Latitude", "Longitude") %>%
  filter(Taxon_a_posteriori %in%
    paste("Physaria",
      c("acutifolia", "brassicoides", "condensata", "dornii",
        "eburniflora", "integrifolia", "vitulifera",
        "medicinae", "chambersii", "rollinsii",
        paste("didymocarpa ssp.", c("didymocarpa", "lanata", "lyrata")),
        paste("saximontana ssp.", c("saximontana", "dentata")),
        paste("floribunda ssp.", c("floribunda", "osterhoutii"))
      )
    ) | grepl("^Physaria$", .data$Taxon_a_posteriori)
  ) %>%
  subset_coords(
    specimen_tbl = .,
    Latitude = c(37, 49.1),
    Longitude = c(-114.5, -102)
  )

FigDiscussionPosteriori <- ggplot() +
  layer_borders(
    spl_extent = spl_bbox(spp_total_posteriori),
    sf_county_color = "black"
  ) +
  layer_specimens(
    specimen_tbl = spp_total_posteriori,
    shape_aes = TRUE,
    id_column = "Taxon_a_posteriori",
    jitter_width = 0.1,
    jitter_height = 0.1,
    jitter_alpha = 0.75
  ) +
  layer_themes(
    specimen_tbl = spp_total_posteriori,
    id_column = "Taxon_a_posteriori",
    legend_title = "Reviewed Annotations"
  ) +
  coord_sf(
    xlim = range(spl_bbox(spp_total_posteriori)[["Longitude"]]),
    ylim = range(spl_bbox(spp_total_posteriori)[["Latitude"]])
  )

FigDiscussionPosteriori <- plot_grid(FigDiscussionPosteriori)

ThesisPackage::save_plot(
  gg_plot = FigDiscussionPosteriori,
  height = 5, width = 6
)
