# Helper Functions ----

#' Arrange Grouped Specimens
#'
#' Order specimens for ggplot layers grouped by identification column. ggplot
#' layers are determined by row number, where groups with higher counts are
#' arranged first.
#'
#' @inheritParams layer_specimens
#' @export
#'
#' @return Tibble data frame sorted by grouped specimen identifications.
#'
#' @examples
#' library(dplyr)
#' distinct(spp_co_front_range, .data$prior_id)
#' spp_co_front_range %>%
#'   group_by(prior_id) %>% tally(x = .)
#' spl_order(specimen_tbl = spp_co_front_range, id_column = "prior_id") %>%
#'   distinct(.data$prior_id)
#'
spl_order <- function(specimen_tbl, id_column) {

  # Group specimens by count of identification column.
  ordered_specimens <- specimen_tbl %>%
    dplyr::group_by_at(dplyr::vars(dplyr::all_of(id_column))) %>%
    dplyr::mutate(id_count = dplyr::n()) %>%
    dplyr::arrange(dplyr::desc(.data$id_count))
  return(ordered_specimens)
}

#' Specimen layer bounding box
#'
#' Calculate bounding box from specimen tibbles or ggplot base layers.
#'
#' @param ... One of specimen tibble or ggplot base layer.
#' @export
#'
#' @return Data frame of Longitude and Latitude bounding box coordinates.
#'
#' @examples
#' spl_bbox(herbarium_specimens)
#'
#' ggmap_ggplot <- layer_ggmap(specimen_tbl = ThesisPackage::spp_co_front_range,
#'                             gg_map_type = "satellite")
#' spl_bbox(ggmap_ggplot)
#' spl_bbox(spp_co_front_range)
#'
spl_bbox <- function(...) {

  spl_base <- list(...)[[1]]

  bbox_extent <- function(bbox) {
    spl_extent <- data.frame(Longitude = c(min(bbox$Longitude),
                                           rep(max(bbox$Longitude), 2),
                                           min(bbox$Longitude)),
                             Latitude = c(rep(min(bbox$Latitude), 2),
                                          rep(max(bbox$Latitude), 2)))
    return(spl_extent)
  }

  # Specimen layer extent for base layer: ggmap
  if (identical(class(spl_base), c("gg", "ggplot"))) {
    spl_extent <- spl_base$data %>%
      dplyr::select(dplyr::matches(c("lon", "[Ll]ongitude", "^x$")),
                    dplyr::matches(c("lat", "[Ll]atitude", "^y$")))
  }

  # Specimen layer extent from specimen tibble
  if (identical(class(spl_base), c("tbl_df", "tbl", "data.frame"))) {
    bbox <- list(
      Longitude = range(spl_base$Longitude, na.rm = TRUE),
      Latitude = range(spl_base$Latitude, na.rm = TRUE)
    )
    spl_extent <- bbox_extent(bbox = bbox)
  }

  # Add consistent names and minimize extent to bounding box points.
  names(spl_extent) <- c("Longitude", "Latitude")
  spl_extent <- bbox_extent(bbox = spl_extent)
  return(spl_extent)
}

#' Bounding box State intersect
#'
#' Build a bounding box polygon simple feature from a spatial extent data frame
#' returned by \link{spl_bbox}. Given a set of state simple features, identify
#' geometry intersections with \link[sf]{st_intersects} to subset state simple
#' features for plotting.
#'
#' @param spl_extent A data frame with specimen layer extent returned by
#'   \link{spl_bbox}
#' @param sf_states State simple features data frame returned by
#'   \link[tigris]{states}
#' @export
#'
#' @return Character vector of states intersecting with spatial extent layer.
#'
#' @examples
#' states_sfs <- tigris::states() %>% rmapshaper::ms_simplify(input = .)
#' spl_bbox(herbarium_specimens) %>%
#'   spl_states(spl_extent = ., sf_states = states_sfs)
#'
spl_states <- function(spl_extent, sf_states) {

  # Build simple features polygon from bounding box extent.
  spl_bbox <- spl_extent %>%
    sf::st_as_sf(coords = c("Longitude", "Latitude"),
                 crs = sf::st_crs("+proj=longlat"))
  spl_polygon <-
    sf::st_polygon(list(as.matrix(rbind(spl_extent, spl_extent[1, ]))))
  spl_geom <- sf::st_sfc(spl_polygon)
  spl_sf <- sf::st_sf(name = "bbox", geometry = spl_geom,
                      crs = "+proj=longlat")
  spl_sf <- sf::st_transform(spl_sf, crs = 3857)

  # Calculate intersection from mercator-projected state polygons.
  sf_states <- sf::st_transform(sf_states, crs = 3857)
  spl_states <- sf_states[sf::st_intersects(spl_sf, sf_states)[[1]], ][["NAME"]]
  return(spl_states)
}


