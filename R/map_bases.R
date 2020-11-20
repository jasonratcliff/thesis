# ggmap Layer ----

#' Layer ggmap base for specimen plot.
#'
#' Build base ggplot layer for specimen mapping over satellite or terrain
#' rasters images. Access to the static maps requires API key registration. To
#' check if the key has been set in the users `.Renviron` file, see
#' `ggmap::has_google_key()` \cr
#' For more information, see the `README` at: https://github.com/dkahle/ggmap
#'
#' @param zoom_lvl Integer passed to `ggmap::get_map()` argument `zoom`.
#' @param gg_map_type Type of map to return from `ggmap::get_map()` function
#'   call. One of: "terrain", "satellite", "roadmap" or "hybrid".
#' @param gg_longitude Numeric vector of length one to optionally center call
#' of `get_map()` longitude coordinates.
#' @param gg_latitude Numeric vector of length one to optionally center call
#' of `get_map()` latitude coordinates.
#' @param ... Forwarded arguments from \link{build_map}
#' @inheritParams layer_specimens
#' @export
#'
#' @return Base ggplot layer from \link{ggmap} raster object.
#'
#' @references
#'  D. Kahle and H. Wickham. ggmap: Spatial Visualization with ggplot2. The R
#'  Journal, 5(1), 144-161. URL
#'  http://journal.r-project.org/archive/2013-1/kahle-wickham.pdf
#'
#' @examples
#' ggmap_ggplot <- layer_ggmap(specimen_tbl = ThesisPackage::spp_co_front_range,
#'                             gg_map_type = "satellite", zoom_lvl = 8)
#' ggmap_ggplot +
#'   layer_borders(spl_extent = spl_bbox(ggmap_ggplot)) +
#'   layer_specimens(specimen_tbl = spp_co_front_range, shape_aes = TRUE,
#'                   id_column = "Taxon_a_posteriori") +
#'   layer_themes(specimen_tbl = spp_co_front_range,
#'                id_column = "Taxon_a_posteriori",
#'                legend_title = "Reviewed Annotations")
#'
layer_ggmap <- function(specimen_tbl, zoom_lvl = 7,
                        gg_map_type = c("terrain", "satellite",
                                        "roadmap", "hybrid"),
                        gg_longitude = NULL, gg_latitude = NULL, ...) {

  # Check registration of Google API key.
  if (ggmap::has_google_key() == FALSE) {
    stop(paste("Register an API key with Google.",
               "See: https://github.com/dkahle/ggmap"))
  }

  # Retrieve ggmap raster from specified coordinates or specimen midrange.
  get_ggmap <- function(locations) {
    ggmap::get_map(location = locations,
                   maptype = gg_map_type, zoom = zoom_lvl,
                   source = "google", messaging = FALSE)
  }
  if (!is.null(gg_longitude) && !is.null(gg_latitude)) {
    ggmap_raster <- get_ggmap(locations = c(gg_longitude, gg_latitude))
  } else if (!is.null(gg_longitude) || !is.null(gg_latitude)) {
    stop("Enter numeric vector for both longitude and latitude coordinates.")
  } else {
    ggmap_raster <-  specimen_tbl %>%
      dplyr::select(.data$Longitude, .data$Latitude) %>%
      purrr::map_dbl(.x = ., ~ (range(.x)[1] + range(.x)[2]) / 2) %>%
      get_ggmap(locations = .)
  }
  ggmap_ggplot <- ggmap::ggmap(ggmap = ggmap_raster)
  return(ggmap_ggplot)
}

# Elevation Layer ----

#' Layer elevation raster for specimen plot.
#'
#' Build base ggplot layer from digital elevation model data from AWS Open Data
#' Terrain Tiles via the \link{elevatr} package.
#' Package repository: https://github.com/jhollist/elevatr
#' Map terrain tiles: https://registry.opendata.aws/terrain-tiles/
#'
#' @param raster_zoom Integer vector of length 1 for zoom level passed to
#' `elevatr::get_elev_raster()` function call.  Must be between 1 and 14.
#' @param raster_factor Integer vector of length 1 for optional aggregation
#' of elevation raster to decrease resolution / object size.
#' @param ... Forwarded arguments from \link{build_map}
#' @inheritParams layer_specimens
#' @export
#'
#' @return Base ggplot layer from \link{elevatr} raster object.
#'
#' @references
#' Hollister, J.W., Tarak Shah (2017). elevatr: Access Elevation Data from
#' Various APIs.
#'
#' @examples
#' library(ggplot2)
#' elev_ggplot <-
#'   layer_elevation(specimen_tbl = ThesisPackage::spp_co_front_range,
#'                   raster_zoom = 7, raster_factor = 1)
#' elev_ggplot +
#'   layer_specimens(specimen_tbl = spp_co_front_range,
#'                   id_column = "Taxon_a_posteriori") +
#'   coord_sf(xlim = range(spl_bbox(elev_ggplot)$Longitude),
#'            ylim = range(spl_bbox(elev_ggplot)$Latitude)) +
#'   labs(x = "Longitude", y = "Latitude")
#'
layer_elevation <- function(specimen_tbl, raster_zoom = 7,
                            raster_factor = 2, ...) {

  # Define projection and get AWS Open Data terrain tiles.
  prj_dd <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  elev_raster <-
    elevatr::get_elev_raster(
      locations = as.data.frame(specimen_tbl[, c("Longitude", "Latitude")]),
      z = raster_zoom, prj = prj_dd, clip = "bbox", src = "aws"
    )
  elev_df <- as.data.frame(methods::as(elev_raster, "SpatialPixelsDataFrame"))
  elev_ggplot <-
    ggplot2::ggplot(elev_df, ggplot2::aes(x = .data$x, y = .data$y)) +
    ggplot2::geom_tile(ggplot2::aes(fill = .data$layer)) +
    ggplot2::scale_fill_gradientn("Elevation (m)",
                                  colours = grDevices::terrain.colors(7),
                                  guide = ggplot2::guide_colourbar(order = 1))
  return(elev_ggplot)
}

