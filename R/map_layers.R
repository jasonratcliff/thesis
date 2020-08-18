# Border Layer ----

#' Add state and county borders `LayerInstance`
#'
#' Build ggplot layer instance for state and county borders.
#' Boundary shapefiles are downloaded from the US Census Bureau
#' Topologically Integrated Geographic Encoding and Referencing (TIGER)
#' database and read into R with the \link{tigris} package (Walker 2016).
#' Data are read in to R as simple features through the \link{sf} package
#' (Pebesma 2018) and transformed with the \link{rmapshaper} package
#' (Teucher and Russell 2020) to simplify border polygons using the
#' Visvalingam algorithm.
#'
#' @param sf_state_color Color of simple features boundry layer.
#' @param sf_county_color Color of simple features boundry layer.
#' @param sf_crs Optional CRS string called by \link[sf]{st_transform}
#' @param ... Forwarded arguments from \link{build_map}
#' @inheritParams spl_states
#' @importFrom ggplot2 geom_sf coord_sf
#' @export
#'
#' @return A ggplot2 `LayerInstance`` for border boundary plotting.
#'
#' @references
#' Pebesma E. 2018. Simple feature for R: Standardized support for spatial
#'   vector data. The R Journal. 10(1):438-446.
#'
#' Teucher A, Russell K. 2020. rmapshaper: Client for 'mapshaper' for
#'   'Geospatial' operations. R package version 0.4.4.
#'    https://CRAN.R-project.org/package=rmapshaper
#'
#' Walker K. 2016. tigris: An R package to access and work with
#'   geographic data from the US census bureau. The R Journal. 8(2):231-242.
#'
#' @examples
#' # tigris::tigris_cache_dir(path = 'PATH TO MY NEW CACHE DIRECTORY')
#' # Sys.getenv('TIGRIS_CACHE_DIR')
#' # options(tigris_class = "sf", tigris_use_cache = TRUE)
#' ggplot2::ggplot() +
#'   layer_borders(spl_extent = spl_bbox(ThesisPackage::spp_co_front_range))
#'
layer_borders <- function(spl_extent, sf_crs = NULL,
                          sf_state_color = "black",
                          sf_county_color = "white", ...) {

  # Build state simple features subset to the specimen layer extent.
  state_sfs <- tigris::states(progress_bar = FALSE) %>%
    rmapshaper::ms_simplify(input = .)
  spp_states <- spl_states(spl_extent = spl_extent,
                           sf_states = state_sfs)
  state_sfs <- state_sfs %>% dplyr::filter(.data$NAME %in% spp_states)

  # Build subset of row-bound county simple features.
  county_sfs <- tigris::rbind_tigris(
    lapply(spp_states, function(state) {
      tigris::counties(state = state, progress_bar = FALSE)
    })
  ) %>% rmapshaper::ms_simplify(input = .)

  # Apply Coordinate Reference System transformation
  if (!is.null(sf_crs)) {
    state_sfs <- sf::st_transform(state_sfs, sf_crs)
    county_sfs <- sf::st_transform(county_sfs, sf_crs)
  }

  # Build ggplot LayerInstance
  border_layer <- list(
    geom_sf(data = county_sfs, inherit.aes = FALSE, size = 0.5, alpha = 0.75,
            color = sf_county_color, fill = NA),
    geom_sf(data = state_sfs, inherit.aes = FALSE, size = 1.2,
            color = sf_state_color, fill = NA)
  )
  return(border_layer)
}

# Specimen Layer ----

#' Add specimen `LayerInstance`
#'
#' Build a ggplot2 layer instance with aesthetics determined by `id_column`
#' matching a variable in `specimen_tbl`. Geom point positions are determined
#' by longitude and latitude coordinates, with color and optional shape
#' aesthetics determined by the values in `id_column`.
#'
#' @param specimen_tbl Tibble of herbarium specimens subset.
#' @param id_column Character scalar matching specimen tibble ID variable.
#'   Variable used for aesthetic (color and shape) mappings.
#' @param legend_status Optional logical vector to show legend via `show.legend`
#'   argument of \link[ggplot2]{geom_point}.
#' @param shape_aes Logical to optionally add shape aesthetic to ggplot.
#' @param geom_size Numeric scalar for size of jitter aesthetic.
#' @importFrom ggplot2 ggplot geom_point aes_
#' @export
#'
#' @return A ggplot2 `LayerInstance`` for specimen plotting.
#'
#' @examples
#' ggplot2::ggplot(data = ThesisPackage::spp_co_front_range) +
#'   layer_specimens(specimen_tbl = ThesisPackage::spp_co_front_range,
#'                   id_column = "Taxon_a_posteriori", shape_aes = FALSE)
#'
#'
layer_specimens <- function(specimen_tbl, id_column, legend_status = TRUE,
                            shape_aes = FALSE,  geom_size = 3) {

  shape_sym <- ifelse(test = shape_aes == FALSE, yes = list(NULL),
                       no = as.name(id_column))  # optional shape aesthetic

  # Arrange specimens for plotting order
  specimen_tbl <- spl_order(specimen_tbl = specimen_tbl,
                            id_column = id_column)
  specimen_layer <-
    list(geom_point(data = specimen_tbl, size = geom_size,
                    show.legend = legend_status,
                    aes_(x = quote(Longitude), y = quote(Latitude),
                         color = as.name(id_column),
                         shape = unlist(shape_sym)), na.rm = TRUE))
  return(specimen_layer)
}

# Map Wrapper ----

#' Build map from layer functions
#'
#' Assemble ggplot from various layer functions. Plot includes a base layer of
#' bare, ggmap, or elevation rasters, county and state borders, specimens, and
#' theme layers. The \link{cowplot} package is used to create a plot grid of the
#' ggplot build.
#'
#' @param map_base Character vector to determine base ggplot layer.
#' @param ... Forward arguments to: \link{layer_ggmap}, \link{layer_elevation},
#'   \link{layer_borders}, \link{layer_specimens}, \link{layer_themes}, and
#'   \link{spl_legend}
#' @inheritParams layer_specimens
#' @inheritParams layer_themes
#' @export
#'
#' @examples
#' specimen_tbl <- spp_co_front_range
#' id_column <- "prior_id"
#' legend_title <- "Priors"
#' map_base <- "base"
#' map_base <- "ggmap"
#' map_base <- "elevation"
#'
build_map <- function(specimen_tbl, id_column, legend_title,
                      map_base = c("base", "ggmap", "elevation"), ...) {

  ggplot_base <-  # ggplot base layer
    switch(map_base, base = ggplot(),
           ggmap = layer_ggmap(specimen_tbl = specimen_tbl, ...),
           elevation = layer_elevation(specimen_tbl = specimen_tbl, ...),
           stop("Invalid `map_base` value"))

  ggplot_extent <-  # Determine extent based on base layer
    switch(map_base, base = spl_bbox(specimen_tbl),
           ggmap = spl_bbox(ggplot_base), elevation = spl_bbox(ggplot_base))

  # Build ggplot from border, specimen layer instances and themes
  ggplot_build <- ggplot_base +
    layer_borders(spl_extent = ggplot_extent, ...) +
    layer_specimens(specimen_tbl = specimen_tbl, id_column = id_column,
                    legend_status = FALSE, shape_aes = TRUE,
                    geom_size = 3) +
    layer_themes(specimen_tbl = specimen_tbl, id_column = id_column,
                 legend_title = legend_title) +
    coord_sf(xlim = range(ggplot_extent[["Longitude"]]),
             ylim = range(ggplot_extent[["Latitude"]]), expand = FALSE)

  # Overwrite HTML markdown theme for discrete / continuous scale conflict.
  if (map_base == "elevation") {
    attributes(ggplot_build$theme$legend.text)$class <-
      c("element_text", "element")
  }

  # Plot grid with
  ggplot_grid <- cowplot::plot_grid(ggplot_build,
    spl_legend(specimen_tbl = specimen_tbl, id_column = id_column,
               shape_aes = TRUE, geom_size = 3, legend_title = legend_title),
    ncol = 1, rel_heights = c(2, 1))

  return(ggplot_grid)
}


