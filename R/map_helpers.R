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
#' ggmap_ggplot <- layer_ggmap(specimen_tbl = Thesis::spp_co_front_range,
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
#' returned by [spl_bbox]. Given a set of state simple features, identify
#' geometry intersections with [st_intersects][sf::st_intersects] to subset state simple
#' features for plotting.
#'
#' @param spl_extent A data frame with specimen layer extent returned by
#'   [spl_bbox]
#' @param sf_states State simple features data frame returned by
#'   [states][tigris::states]
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

# Specimen ID ----

#' Map Individual Specimen
#'
#' @description 
#' `r lifecycle::badge("deprecated")`
#' - See `inst/scripts/IntroCarbonWyo.R` for implementation by call expression.
#' 
#' Generate geom specifications to add label of individual specimen(s).
#'
#' @param h_adjust Numeric vector of length one for horizontal label adjustment.
#' @param v_adjust Numeric vector of length one for vertical label adjustment.
#' @param label_size Numeric vector of length one for label size.
#' @inheritParams layer_specimens
#' @inheritParams layer_themes
#' @inheritParams find_spp
#' @export
#'
#' @return ggplot object with added specimen annotation layer.
#'
#' @examples
#' ggplot2::ggplot() +
#'   layer_specimens(specimen_tbl = spp_co_front_range,
#'                   id_column = "prior_id", shape_aes = "prior_id") +
#'   spl_id(specimen_tbl = spp_co_front_range,
#'          id_column = "prior_id", shape_aes = "prior_id",
#'          collector = "Wolf", collection = 642)
#'
spl_id <- function(specimen_tbl, id_column, collector, collection,
                   shape_aes = NULL,  geom_size = 3, label_size = 3,
                   h_adjust = 0.25, v_adjust = -0.15) {

  # Call `find_spp()` function to get specimen annotation data.
  spp_id <- Thesis::find_spp(specimen_tbl = specimen_tbl,
                                    collector = collector,
                                    collection = collection) %>%
    dplyr::mutate(
      # Remove collector initials
      taxa_label = stringr::str_remove_all(
        string = .data$Collector,
        pattern = "[A-Z]\\. ?") %>%
        stringr::str_replace(string = ., pattern = "and|with",
                             replacement =  "&") %>%
        paste(., .data$Collection_Number, collapse = "", sep = "\n")
      ) %>%
    # Account for duplicate label matches from joining.
    dplyr::group_by(.data$taxa_label) %>% dplyr::slice(1) %>% dplyr::ungroup()

  # Plot additional map layer to include the specimen returned by spp_find().
  spl_ids <- list(
    ggplot2::geom_point(
      data = spp_id, inherit.aes = FALSE, show.legend = FALSE,
      mapping = ggplot2::aes(x = .data$Longitude, y = .data$Latitude),
      size = 5, shape = 5, fill = NA),
    ggplot2::geom_point(
      data = spp_id, inherit.aes = FALSE, size = geom_size,
      mapping = ggplot2::aes_string(x = "Longitude", y = "Latitude",
                                    colour = id_column, shape = shape_aes)),
    ggplot2::geom_label(
      data = spp_id, inherit.aes = FALSE, alpha = 0.5,
      nudge_x = h_adjust, nudge_y = v_adjust, size = label_size,
      mapping = ggplot2::aes(x = .data$Longitude, y = .data$Latitude,
                             label = .data$taxa_label),
      label.padding = ggplot2::unit(0.1, "lines"),
     )
  )

  return(spl_ids)
}

