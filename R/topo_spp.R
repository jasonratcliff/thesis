# Load dependencies
library(ggmap)

topo_spp <- function(topo_frame, size,
                     map_type = c("terrain", "satellite", "roadmap", "hybrid"),
                     longitude = NULL, latitude = NULL) {
  # Function to build satellite and terrain maps for ggplot base layer
  # 
  # Args:
  #   topo_frame: Data frame of specimens from which to extract coordinates.
  #     Missing values are removed and mean values calculated.
  #   map_type: Character vector for MAPTYPE argument of ggmap::getmap(). 
  #     Should match “terrain”, “satellite”, “roadmap”, or “hybrid”.
  #   size: Numeric vector for ZOOM argument of ggmap::getmap().
  #     Scales lon / lat axes for ggmap base.
  #   longitude: When !is.null, centers LOCATION argument of ggmap::getmap().
  #   latitude: When !is.null, centers LOCATION argument of ggmap::getmap().
  #     LONGITUDE and LATITUDE must both be non-null to generate centered map.
  
  # Subset of TOPO_FRAME for which long / lat coordinates are not NA.
  topo_coord <- topo_frame[which(!is.na(topo_frame$Longitude) &
                                   !is.na(topo_frame$Latitude)), 
                           c("Longitude", "Latitude")]
  
  # If both LONGITUDE and LATITUDE are NULL, calculate TOPO_MEAN.
  if (is.null(longitude) & is.null(latitude)) {
    topo_mean <- sapply(topo_coord[, c("Longitude", "Latitude") ], mean)  
    topo_base <- get_map(location = topo_mean, maptype = map_type, 
                         zoom = size, source = "google")
  }
  
  # If LONGITUDE and LATITUDE are non-null, get coordinate specified map.
  else if (!is.null(longitude) & !is.null(latitude)) {
        topo_base <- get_map(location = c(longitude, latitude),
                         maptype = map_type, zoom = size, source = "google")
  }
  return(topo_base)
}
