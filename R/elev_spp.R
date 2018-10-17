require(grDevices)
require(elevatr)
require(rgdal)
require(raster)
require(sp)
require(ggplot2)
require(ggmap)

# Function arguments
#   specimens: Data frame of herbarium records to plot elevation profile.
#   raster_zoom: Integer between 1 and 14 passed to elevatr::get_elev_raster
#   raster_factor: Optional call of raster::aggregate to factor resolution
#   map_borders: Color aesthetic for border polygon geoms.
#
elev_spp <- function(specimens, raster_zoom, raster_factor = 2, 
                     map_borders = "black", geom_size = 3,
                     spp_id = "Taxon_a_posteriori", 
                     lengend_title = "Reviewed Annotations") {
  
  # Set API from Nextzen (https://www.nextzen.org/)
  elevatr_aws_api <- "wVm9mkIHRLOPQ3zlWwT3DA"
  
  # Subset coordinate vectors to calculate range of plot bounding box.
  raster_bbx <- specimens[, c("Longitude", "Latitude")]
  names(raster_bbx) <- c("x", "y")

  # Extract range limits as build data frame.
  raster_index <- as.data.frame(matrix(range(raster_bbx$x), ncol = 1))
  raster_index <- cbind(raster_index, matrix(range(raster_bbx$y), ncol = 1))
  names(raster_index) <- c("x", "y")

  # Define projection
  prj_dd <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

  # Get AWS Open Data Terrain Tiles
  # Cite: https://registry.opendata.aws/terrain-tiles/
  specimen_elev_raster <- get_elev_raster(locations = raster_bbx, 
                                           z = raster_zoom, prj = prj_dd, 
                                           src = "aws", api_key = elevatr_aws_api)
  
  # Use "raster" package function aggregate() to decrease data resolution and
  # reduce the size / memory of elevation raster objects (Hijmans 2017).
  # Cite: http://pakillo.github.io/R-GIS-tutorial/#resolution
  if (raster_factor > 1) { 
    specimen_elev_raster <- aggregate(specimen_elev_raster,
                                      fact = raster_factor, fun = mean)
  }

  # Calculate border extents
  map_xlim <- c(raster_index$x[1], raster_index$x[2])
  map_ylim <- c(raster_index$y[1], raster_index$y[2])

  # Use "sp" package to define a spatial grid from the raster layer.
  # Cite: https://groups.google.com/forum/#!msg/ggplot2/9fS4OfHEQq8/ZafTyvVKfJIJ
  spp_elev_spdf <- as(specimen_elev_raster, "SpatialPixelsDataFrame")
  spp_elev_df <- as.data.frame(spp_elev_spdf)
  
  # Subset raster to specimen records.
  spp_elev_df <- spp_elev_df[spp_elev_df$x > map_xlim[1], ]
  spp_elev_df <- spp_elev_df[spp_elev_df$x < map_xlim[2], ]
  spp_elev_df <- spp_elev_df[spp_elev_df$y > raster_index$y[1], ]
  spp_elev_df <- spp_elev_df[spp_elev_df$y < raster_index$y[2], ]
  
  # Return data frames for state and county borders.
  mapped_states <- map_data("state")
  mapped_counties <- map_data("county")
  
  # Subset plotting data to remove extra data and missing values.
  spp_plot <- specimens[, c(spp_id, "Longitude", "Latitude")]
  spp_plot <- spp_plot[which(!is.na(spp_plot[, spp_id])), ]
  geom_size_corr <- geom_size + 0.25

  # ggplot elevation projection with county & state borders.
  spp_elev_ggplot <- ggplot(spp_elev_df, aes(x=x, y=y)) +
    geom_tile(aes(fill = layer)) + 
    geom_polygon(data = mapped_counties, 
                 mapping = aes(x = long, y = lat, group = group),
                 color = map_borders, fill = NA, size = .25) +
    geom_polygon(data = mapped_states, 
                 mapping = aes(x = long, y = lat, group = group),
                 color = map_borders, fill = NA, size = 1.5)  +
    geom_point(data = spp_plot, size = geom_size_corr, show.legend = FALSE,
               aes(x = Longitude, y = Latitude), colour = "black", alpha = 0.2) +
    geom_point(data = spp_plot, size = geom_size, na.rm = TRUE,
               aes(x = Longitude, y = Latitude, 
                   colour = get(spp_id), shape = get(spp_id))) +
    scale_x_continuous("Longitude") +
    scale_y_continuous("Latitude") +
    scale_color_manual(lengend_title, values = spp_color) +
    scale_shape_manual(lengend_title, values = spp_shape) +
    coord_equal(xlim = map_xlim, ylim = map_ylim, expand = FALSE) +
    scale_fill_gradientn("Elevation (m)", colours = terrain.colors(7)) +
    theme(panel.grid = element_blank(), panel.background = element_blank(),
          legend.direction = "vertical", legend.position = "bottom")
    
  # Return ggplot built from elevation tile raster, border, and specimen geoms.
  return(spp_elev_ggplot)
}
