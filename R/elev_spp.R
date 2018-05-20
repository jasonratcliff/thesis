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
elev_spp <- function(specimens, raster_zoom = 7, raster_factor = 2, 
                     map_borders = "black", jitter_geoms = TRUE, 
                     geom_size = 3, jitter_pos = c(0.02, 0.02)) {
  
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
 
  # ggplot elevation projection with county & state borders.
  spp_elev_ggplot <- ggplot(spp_elev_df, aes(x=x, y=y)) +
    geom_tile(aes(fill = layer)) + 
    geom_polygon(data = mapped_counties, 
                 mapping = aes(x = long, y = lat, group = group),
                 color = map_borders, fill = NA, size = .25) +
    geom_polygon(data = mapped_states, 
                 mapping = aes(x = long, y = lat, group = group),
                 color = map_borders, fill = NA, size = 1.5)  +
    # coord_equal(xlim = map_xlim, ylim = map_ylim, expand = FALSE) + 
    coord_equal(xlim = map_xlim, ylim = map_ylim, expand = FALSE) +
    scale_fill_gradientn("Elevation", colours = terrain.colors(7))
  
  # Add geom layer of specimen occurrences and format scales.
  spp_elev_ggplot <- spp_elev_ggplot + 
    # geom_point(data = specimens, 
    #            aes(Longitude, Latitude), size = 3.25, shape = 18) +
    geom_jitter(data = specimens, na.rm = TRUE,
                aes(x = Longitude, y = Latitude,
                    colour = Taxon_a_posteriori, shape = Taxon_a_posteriori),
                size = geom_size, width = jitter_pos[1], height = jitter_pos[2]) +
    scale_x_continuous("Longitude") +
    scale_y_continuous("Latitude") +
    scale_color_manual("Reviewed Annotations", values = spp_color) + 
    scale_shape_manual("Reviewed Annotations", values = spp_shape) + 
    
    # Global legend theme, guides and limits for ggmaps.
    theme(panel.grid = element_blank(), panel.background = element_blank(),
          legend.direction = "vertical", legend.position = "bottom") + 
    
    guides(colour = guide_legend(ncol = 1))
  
      # legend.box = "vertical", , 
      #     , legend.title.align = 0.5,
         # ) + 
    
    # theme(legend.position = c(0, .75), 
    #       legend.justification = c(0, 1), 
    #       legend.direction = "vertical") + 
  
  # ggmap_themes <- theme(legend.box = "vertical", legend.position = "bottom", 
  #                       legend.direction = "vertical", legend.title.align = 0.5)
  # ggmap_guides <- guides(colour = guide_legend(ncol = 2, byrow = TRUE))

  
  
  # Return ggplot built from elevation tile raster, border, and specimen geoms.
  return(spp_elev_ggplot)
}
