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

# Map Base ---------------------------------------------------------------------

#' Layer ggmap base for specimen plot.
#'
#' @description
#' `r lifecycle::badge("deprecated")`
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
#' @param ... Forwarded arguments from [build_map]
#' @inheritParams layer_specimens
#' @export
#'
#' @return Base ggplot layer from [ggmap] raster object.
#'
#' @references
#'  D. Kahle and H. Wickham. ggmap: Spatial Visualization with ggplot2. The R
#'  Journal, 5(1), 144-161. URL
#'  http://journal.r-project.org/archive/2013-1/kahle-wickham.pdf
#'
#' @examples
#' ggmap_ggplot <- layer_ggmap(
#'   specimen_tbl = Thesis::spp_co_front_range,
#'   gg_map_type = "satellite", zoom_lvl = 8
#' )
#' ggmap_ggplot +
#'   layer_borders(spl_extent = spl_bbox(ggmap_ggplot)) +
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
layer_ggmap <- function(specimen_tbl, zoom_lvl = 7,
                        gg_map_type = c(
                          "terrain", "satellite",
                          "roadmap", "hybrid"
                        ),
                        gg_longitude = NULL, gg_latitude = NULL, ...) {

  # Check registration of Google API key.
  if (ggmap::has_google_key() == FALSE) {
    stop(paste(
      "Register an API key with Google.",
      "See: https://github.com/dkahle/ggmap"
    ))
  }

  # Retrieve ggmap raster from specified coordinates or specimen midrange.
  get_ggmap <- function(locations) {
    ggmap::get_map(
      location = locations,
      maptype = gg_map_type, zoom = zoom_lvl,
      source = "google", messaging = FALSE
    )
  }
  if (!is.null(gg_longitude) && !is.null(gg_latitude)) {
    ggmap_raster <- get_ggmap(locations = c(gg_longitude, gg_latitude))
  } else if (!is.null(gg_longitude) || !is.null(gg_latitude)) {
    stop("Enter numeric vector for both longitude and latitude coordinates.")
  } else {
    ggmap_raster <- specimen_tbl %>%
      dplyr::select(.data$Longitude, .data$Latitude) %>%
      purrr::map_dbl(.x = ., ~ (range(.x)[1] + range(.x)[2]) / 2) %>%
      get_ggmap(locations = .)
  }
  ggmap_ggplot <- ggmap::ggmap(ggmap = ggmap_raster)
  return(ggmap_ggplot)
}

#' Layer elevation raster for specimen plot.
#'
#' @description
#' `r lifecycle::badge("deprecated")`
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
#'   layer_elevation(
#'     specimen_tbl = Thesis::spp_co_front_range,
#'     raster_zoom = 7, raster_factor = 1
#'   )
#' elev_ggplot +
#'   layer_specimens(
#'     specimen_tbl = spp_co_front_range,
#'     id_column = "Taxon_a_posteriori"
#'   ) +
#'   coord_sf(
#'     xlim = range(spl_bbox(elev_ggplot)$Longitude),
#'     ylim = range(spl_bbox(elev_ggplot)$Latitude)
#'   ) +
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
      guide = ggplot2::guide_colourbar(order = 1)
    )
  return(elev_ggplot)
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

# Specimen ID ------------------------------------------------------------------

#' Map Individual Specimen
#'
#' @description
#' `r lifecycle::badge("deprecated")`
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
#'   layer_specimens(
#'     specimen_tbl = spp_co_front_range,
#'     id_column = "prior_id", shape_aes = "prior_id"
#'   ) +
#'   spl_id(
#'     specimen_tbl = spp_co_front_range,
#'     id_column = "prior_id", shape_aes = "prior_id",
#'     collector = "Wolf", collection = 642
#'   )
#'
spl_id <- function(specimen_tbl, id_column, collector, collection,
                   shape_aes = NULL, geom_size = 3, label_size = 3,
                   h_adjust = 0.25, v_adjust = -0.15) {

  # Call `find_spp()` function to get specimen annotation data.
  spp_id <- Thesis::find_spp(
    specimen_tbl = specimen_tbl,
    collector = collector,
    collection = collection
  ) %>%
    dplyr::mutate(
      # Remove collector initials
      taxa_label = stringr::str_remove_all(
        string = .data$Collector,
        pattern = "[A-Z]\\. ?"
      ) %>%
        stringr::str_replace(
          string = ., pattern = "and|with",
          replacement = "&"
        ) %>%
        paste(., .data$Collection_Number, collapse = "", sep = "\n")
    ) %>%
    # Account for duplicate label matches from joining.
    dplyr::group_by(.data$taxa_label) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()

  # Plot additional map layer to include the specimen returned by spp_find().
  spl_ids <- list(
    ggplot2::geom_point(
      data = spp_id, inherit.aes = FALSE, show.legend = FALSE,
      mapping = ggplot2::aes(x = .data$Longitude, y = .data$Latitude),
      size = 5, shape = 5, fill = NA
    ),
    ggplot2::geom_point(
      data = spp_id, inherit.aes = FALSE, size = geom_size,
      mapping = ggplot2::aes_string(
        x = "Longitude", y = "Latitude",
        colour = id_column, shape = shape_aes
      )
    ),
    ggplot2::geom_label(
      data = spp_id, inherit.aes = FALSE, alpha = 0.5,
      nudge_x = h_adjust, nudge_y = v_adjust, size = label_size,
      mapping = ggplot2::aes(
        x = .data$Longitude, y = .data$Latitude,
        label = .data$taxa_label
      ),
      label.padding = ggplot2::unit(0.1, "lines"),
    )
  )

  return(spl_ids)
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
