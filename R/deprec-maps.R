# Layers -----------------------------------------------------------------------

#' Add state and county borders `LayerInstance`
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' Build ggplot layer instance for state and county borders.
#' Boundary shapefiles are downloaded from the US Census Bureau
#' Topologically Integrated Geographic Encoding and Referencing (TIGER)
#' database and read into R with the [tigris] package (Walker 2016).
#' Data are read in to R as simple features through the [sf] package
#' (Pebesma 2018) and transformed with the [rmapshaper] package
#' (Teucher and Russell 2020) to simplify border polygons using the
#' Visvalingam algorithm.
#'
#' @param sf_state_color Color of simple features boundry layer.
#' @param sf_county_color Color of simple features boundry layer.
#' @param sf_crs Optional CRS string called by [st_transform][sf::st_transform]
#' @param ... Forwarded arguments from [build_map]
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
#'   layer_borders(spl_extent = spl_bbox(Thesis::spp_co_front_range))
#'
layer_borders <- function(spl_extent, sf_crs = NULL,
                          sf_state_color = "black",
                          sf_county_color = "white", ...) {

  # Build state simple features subset to the specimen layer extent.
  state_sfs <- tigris::states(progress_bar = FALSE) %>%
    rmapshaper::ms_simplify(input = .)
  spp_states <- spl_states(
    spl_extent = spl_extent,
    sf_states = state_sfs
  )
  state_sfs <- state_sfs %>%
    dplyr::filter(.data$NAME %in% spp_states)

  # Build subset of row-bound county simple features.
  county_sfs <- tigris::rbind_tigris(
    lapply(spp_states, function(state) {
      tigris::counties(state = state, progress_bar = FALSE)
    })
  ) %>%
    rmapshaper::ms_simplify(input = .)

  # Apply Coordinate Reference System transformation
  if (!is.null(sf_crs)) {
    state_sfs <- sf::st_transform(state_sfs, sf_crs)
    county_sfs <- sf::st_transform(county_sfs, sf_crs)
  }

  # Build ggplot LayerInstance
  border_layer <- list(
    geom_sf(
      data = county_sfs, inherit.aes = FALSE, size = 0.5, alpha = 0.75,
      color = sf_county_color, fill = NA
    ),
    geom_sf(
      data = state_sfs, inherit.aes = FALSE, size = 1.2,
      color = sf_state_color, fill = NA
    )
  )
  return(border_layer)
}

#' Add specimen `LayerInstance`
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' Build a ggplot2 layer instance with aesthetics determined by `id_column`
#' matching a variable in `specimen_tbl`. Geom point positions are determined
#' by longitude and latitude coordinates, with color and optional shape
#' aesthetics determined by the values in `id_column`.
#'
#' @param specimen_tbl Tibble of herbarium specimens subset.
#' @param id_column Character scalar matching specimen tibble ID variable.
#'   Variable used for aesthetic (color and shape) mappings.
#' @param legend_status Optional logical vector to show legend via `show.legend`
#'   argument of [geom_point][ggplot2::geom_point].
#' @param shape_aes Logical to optionally add shape aesthetic to ggplot.
#' @param geom_size Numeric scalar for size of jitter aesthetic.
#' @param jitter_width Numeric scalar for [geom_jitter()] width.
#' @param jitter_height Numeric scalar for [geom_jitter()] height.
#' @param jitter_alpha Numeric scalar for [geom_jitter()] alpha value.
#' @importFrom ggplot2 ggplot geom_jitter aes_
#' @export
#'
#' @return A ggplot2 `LayerInstance`` for specimen plotting.
#'
#' @examples
#' ggplot2::ggplot() +
#'   layer_specimens(
#'     specimen_tbl = Thesis::spp_co_front_range,
#'     id_column = "Taxon_a_posteriori", shape_aes = FALSE,
#'     jitter_width = 0.033, jitter_height = 0.033
#'   )
#'
layer_specimens <- function(specimen_tbl, id_column, legend_status = TRUE,
                            shape_aes = FALSE, geom_size = 3,
                            jitter_width = NULL, jitter_height = NULL,
                            jitter_alpha = 1) {
  shape_sym <- ifelse(test = shape_aes == FALSE, yes = list(NULL),
    no = as.name(id_column)
  ) # optional shape aesthetic

  # Arrange specimens for plotting order
  specimen_tbl <- spl_order(
    specimen_tbl = specimen_tbl,
    id_column = id_column
  )
  specimen_layer <-
    list(
      geom_jitter(
        data = specimen_tbl, size = geom_size,
        show.legend = legend_status,
        aes_(
          x = quote(Longitude), y = quote(Latitude),
          color = as.name(id_column),
          shape = unlist(shape_sym)
        ), na.rm = TRUE,
        width = jitter_width,
        height = jitter_height,
        alpha = jitter_alpha
      )
    )
  return(specimen_layer)
}

#' Add map ggplot scales and themes
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' Add manual values to specimen identification discrete scales in map ggplots
#' set by [spp_color] and [spp_shape] vectors. Modify plot themes
#' and add markdown italics for specific epithets with [spl_labels].
#'
#' @param legend_title Character vector of length one to set the ggplot
#'   legend title.
#' @inheritParams layer_specimens
#' @export
#'
#' @return List of scale layers (colour, shape), themes, and axis labels.
#'
#' @examples
#' # Add theme specifications and markdown legend.
#' ggplot2::ggplot() +
#'   layer_borders(
#'     spl_extent = spl_bbox(spp_co_front_range),
#'     sf_county_color = "black"
#'   ) +
#'   layer_specimens(
#'     specimen_tbl = spp_co_front_range, shape_aes = TRUE,
#'     id_column = "Taxon_a_posteriori"
#'   ) +
#'   layer_themes(
#'     specimen_tbl = spp_co_front_range,
#'     id_column = "Taxon_a_posteriori",
#'     legend_title = "Reviewed Annotations"
#'   )
#'
layer_themes <- function(specimen_tbl, id_column, legend_title) {

  # Assign HTML markdown label vector.
  markdown_labels <-
    Thesis::spl_labels(
      specimen_tbl = specimen_tbl,
      id_column = id_column
    )

  # Build ggplot scale and theme layers.
  theme_layer <- list(
    ggplot2::scale_color_manual(
      name = legend_title,
      labels = markdown_labels,
      limits = names(markdown_labels),
      values = Thesis::spp_color,
      na.value = "black", drop = TRUE
    ),
    ggplot2::scale_shape_manual(
      name = legend_title,
      labels = markdown_labels,
      limits = names(markdown_labels),
      values = Thesis::spp_shape,
      na.value = 17, drop = TRUE
    ),
    ggplot2::theme(
      panel.background = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(
        fill = NA,
        color = "black"
      ),
      legend.text.align = 0, legend.title.align = 0.5,
      legend.direction = "vertical",
      legend.key = ggplot2::element_blank(),
      legend.background = ggplot2::element_rect(
        fill = "grey90",
        color = "black"
      ),
      legend.text = ggtext::element_markdown()
    ),
    ggplot2::labs(
      x = "Longitude", y = "Latitude"
    )
  )
  return(theme_layer)
}

# Helpers ----------------------------------------------------------------------

#' Bounding box State intersect
#'
#' @description
#' `r lifecycle::badge("deprecated")`
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
    sf::st_as_sf(
      coords = c("Longitude", "Latitude"),
      crs = sf::st_crs("+proj=longlat")
    )
  spl_polygon <-
    sf::st_polygon(list(as.matrix(rbind(spl_extent, spl_extent[1, ]))))
  spl_geom <- sf::st_sfc(spl_polygon)
  spl_sf <- sf::st_sf(
    name = "bbox", geometry = spl_geom,
    crs = "+proj=longlat"
  )
  spl_sf <- sf::st_transform(spl_sf, crs = 3857)

  # Calculate intersection from mercator-projected state polygons.
  sf_states <- sf::st_transform(sf_states, crs = 3857)
  spl_states <- sf_states[sf::st_intersects(spl_sf, sf_states)[[1]], ][["NAME"]]
  return(spl_states)
}

#' Arrange Grouped Specimens
#'
#' @description
#' `r lifecycle::badge("deprecated")`
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
#'   group_by(prior_id) %>%
#'   tally(x = .)
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
#' @description
#' `r lifecycle::badge("deprecated")`
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
#' ggmap_ggplot <- layer_ggmap(
#'   specimen_tbl = Thesis::spp_co_front_range,
#'   gg_map_type = "satellite"
#' )
#' spl_bbox(ggmap_ggplot)
#' spl_bbox(spp_co_front_range)
#'
spl_bbox <- function(...) {
  spl_base <- list(...)[[1]]

  bbox_extent <- function(bbox) {
    spl_extent <- data.frame(
      Longitude = c(
        min(bbox$Longitude),
        rep(max(bbox$Longitude), 2),
        min(bbox$Longitude)
      ),
      Latitude = c(
        rep(min(bbox$Latitude), 2),
        rep(max(bbox$Latitude), 2)
      )
    )
    return(spl_extent)
  }

  # Specimen layer extent for base layer: ggmap
  if (identical(class(spl_base), c("gg", "ggplot"))) {
    spl_extent <- spl_base$data %>%
      dplyr::select(
        dplyr::matches(c("lon", "[Ll]ongitude", "^x$")),
        dplyr::matches(c("lat", "[Ll]atitude", "^y$"))
      )
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

#' Extract specimen layer legend
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' Build a minimal specimen layer ggplot with themes and extract the legend.
#'
#' @inheritParams layer_specimens
#' @inheritParams layer_themes
#' @export
#'
#' @examples
#' cowplot::plot_grid(
#'   spl_legend(
#'     specimen_tbl = spp_co_front_range,
#'     id_column = "Taxon_a_posteriori",
#'     legend_title = "Review Annotations",
#'     shape_aes = TRUE, geom_size = 3
#'   )
#' )
#'
spl_legend <- function(specimen_tbl, id_column, legend_title,
                       shape_aes, geom_size) {
  gg_legend <-
    cowplot::get_legend(
      plot = ggplot2::ggplot() +
        layer_specimens(
          specimen_tbl = specimen_tbl, id_column = id_column,
          shape_aes = shape_aes, geom_size = geom_size
        ) +
        layer_themes(
          specimen_tbl = specimen_tbl, id_column = id_column,
          legend_title = legend_title
        ) +
        ggplot2::guides(colour = ggplot2::guide_legend(ncol = 2))
    )
  return(gg_legend)
}

# Wrapper ----------------------------------------------------------------------

#' Build map from layer functions
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' Assemble ggplot from various layer functions. Plot includes a base layer of
#' bare, ggmap, or elevation rasters, county and state borders, specimens, and
#' theme layers. The [cowplot] package is used to create a plot grid of the
#' ggplot build.
#'
#' @param map_base Character vector to determine base ggplot layer.
#' @param ... Forward arguments to: [layer_ggmap], [layer_elevation],
#'   [layer_borders], [layer_specimens], [layer_themes], and
#'   [spl_legend]
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
  ggplot_base <- # ggplot base layer
    switch(map_base,
      base = ggplot(),
      ggmap = layer_ggmap(specimen_tbl = specimen_tbl, ...),
      elevation = layer_elevation(specimen_tbl = specimen_tbl, ...),
      stop("Invalid `map_base` value")
    )

  ggplot_extent <- # Determine extent based on base layer
    switch(map_base,
      base = spl_bbox(specimen_tbl),
      ggmap = spl_bbox(ggplot_base),
      elevation = spl_bbox(ggplot_base)
    )

  # Build ggplot from border, specimen layer instances and themes
  ggplot_build <- ggplot_base +
    layer_borders(spl_extent = ggplot_extent, ...) +
    layer_specimens(
      specimen_tbl = specimen_tbl, id_column = id_column,
      legend_status = FALSE, shape_aes = TRUE, geom_size = 3
    ) +
    layer_themes(
      specimen_tbl = specimen_tbl, id_column = id_column,
      legend_title = legend_title
    ) +
    coord_sf(
      xlim = range(ggplot_extent[["Longitude"]]),
      ylim = range(ggplot_extent[["Latitude"]]), expand = FALSE
    )

  # Overwrite HTML markdown theme for discrete / continuous scale conflict.
  if (map_base == "elevation") {
    attributes(ggplot_build$theme$legend.text)$class <-
      c("element_text", "element")
  }

  # Plot grid with
  ggplot_grid <- cowplot::plot_grid(ggplot_build,
    spl_legend(
      specimen_tbl = specimen_tbl, id_column = id_column,
      shape_aes = TRUE, geom_size = 3, legend_title = legend_title
    ),
    ncol = 1, rel_heights = c(2, 1)
  )

  return(ggplot_grid)
}
