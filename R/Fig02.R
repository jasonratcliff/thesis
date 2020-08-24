library(ThesisPackage)
library(ggplot2)
options(tigris_use_cache = TRUE)

# Figure 2 ----
# - Introduction: Build ggmap of putative *P. vitulifera* in Carbon County, WY.
carbon_wyo <- list()

# Row tibble for text annotation of Bears Ears Range area.
carbon_wyo$bears_ear <-
  tibble::tribble(
    ~"Feature", ~"Longitude", ~"Latitude",
    "Bears Ears Range", -106.05, 40.275,
    "North\nPark", -106.25, 40.75,
    "Middle Park", -105.94, 40.11
  )

# Subset specimens to add map labels.
carbon_wyo$labels <- spp_carbon_wyo %>%
  dplyr::filter(grepl("Kastning|Nelson", Collector)) %>%
  dplyr::filter(grepl("1462|1725|49286|49478", Collection_Number))

carbon_wyo$spp <- spp_carbon_wyo %>%
  dplyr::filter(!grepl("Kastning|Nelson", Collector) &
                  !grepl("1462|1725|49286|49478", Collection_Number))

# Satellite map of Carbon, WY specimens.
Fig02 <-
  layer_ggmap(specimen_tbl = spp_carbon_wyo, gg_map_type = "satellite",
              gg_longitude = -106.5, gg_latitude = 40.4, zoom_lvl = 8) + 
  layer_borders(spl_extent = spl_bbox(spp_carbon_wyo),
                sf_county_color = "black") + 
  layer_specimens(specimen_tbl = spp_carbon_wyo, shape_aes = TRUE, 
                  id_column = "Taxon_a_posteriori", legend_status = TRUE) +
  layer_themes(specimen_tbl = spp_carbon_wyo, id_column = "Taxon_a_posteriori",
               legend_title = "Reviewed Annotations") +
  geom_text(data = carbon_wyo$bears_ear,
            aes(Longitude, Latitude, label = Feature, group = NULL),
            color = "white", size = 3.5) +
  
  spl_id(specimen_tbl = carbon_wyo$labels,
         id_column = "Taxon_a_posteriori", shape_aes = "Taxon_a_posteriori", 
         collector = "Kastning", collection = 1462,
         h_adjust = -0.425, v_adjust = -0.15) +
  
  spl_id(specimen_tbl = carbon_wyo$labels,
         id_column = "Taxon_a_posteriori", shape_aes = "Taxon_a_posteriori",
         collector = "Kastning", collection = 1725,
         h_adjust = -0.4, v_adjust = -0.15
  ) + 
  
  spl_id(specimen_tbl = carbon_wyo$labels,
         id_column = "Taxon_a_posteriori", shape_aes = "Taxon_a_posteriori",
         collector = "Nelson", collection = 49286,
         h_adjust = 0.275, v_adjust = -0.1
  ) + 
  
  spl_id(specimen_tbl = carbon_wyo$labels,
         id_column = "Taxon_a_posteriori", shape_aes = "Taxon_a_posteriori",
         collector = "Nelson", collection = 49478,
         h_adjust = -0.225, v_adjust = -0.12
  ) +
  
  theme(legend.direction = "vertical", legend.position = "bottom") +
  
  guides(colour = guide_legend(ncol = 2, byrow = TRUE))

ThesisPackage::save_plot(gg_plot = Fig02)
