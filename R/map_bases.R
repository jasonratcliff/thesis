# Elevation Layer ----

#' Layer elevation raster for specimen plot.
#'
#' Build base ggplot layer from digital elevation model data from AWS Open Data
#' Terrain Tiles via the [elevatr] package.
#' Package repository: https://github.com/jhollist/elevatr
#' Map terrain tiles: https://registry.opendata.aws/terrain-tiles/
#'
#' @param raster_zoom Integer vector of length 1 for zoom level passed to
#' `elevatr::get_elev_raster()` function call.  Must be between 1 and 14.
#' @param raster_factor Integer vector of length 1 for optional aggregation
#' of elevation raster to decrease resolution / object size.
#' @param ... Forwarded arguments from [build_map]
#' @inheritParams layer_specimens
#' @export
#'
#' @return Base ggplot layer from [elevatr] raster object.
#'
#' @references
#' Hollister, J.W., Tarak Shah (2017). elevatr: Access Elevation Data from
#' Various APIs.
#'
#' @examples
#' library(ggplot2)
#' elev_ggplot <-
#'   layer_elevation(specimen_tbl = Thesis::spp_co_front_range,
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
    ggplot2::geom_tile(ggplot2::aes(fill = elev_df[, 1])) +
    ggplot2::scale_fill_gradientn("Elevation (m)",
                                  colours = grDevices::terrain.colors(7),
                                  guide = ggplot2::guide_colourbar(order = 1))
  return(elev_ggplot)
}
