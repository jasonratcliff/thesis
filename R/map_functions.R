library(ggplot2)
library(ggtext)
library(ggmap)
library(elevatr)

# Build Border Layer ----

#' Build ggplot layer of state and county borders.
#'
#' Utilize US Department of Commerce Census Bureau data to plot state and county
#' boundaries using the `ggplot2::map_data()` extension of the `maps` package.
#'
#' @param border_color Text value for `geom_polygon` color aesthetic mapping.
#' @param border_fill Text value for `geom_polygon` fill aesthetic mapping.
#'   Default is set to `NA` for a transparent fill of county polygons.
#' @param border_size_county Numeric value for county border size aesthetic.
#' @param border_size_state Numeric value for state border size aesthetic.
#' @return Object of `ggplot` class with polygon layers of state and county
#' borders.
map_borders <- function(border_color, border_fill = NA,
                        border_size_county = .125,
                        border_size_state = 1.5) {

  # Define vector of states / regions to get boundary geoms.
  border_regions <- c("Montana", "Wyoming", "Colorado",
                      "Utah", "Idaho", "Nebraska",
                      "North Dakota", "South Dakota",
                      "New Mexico", "Arizona", "Nevada",
                      "Washington", "Oregon", "California")

  # Initialize ggplot base map layer of county lines with state borders.
  border_states <- map_data("state", region = border_regions)
  border_counties <- map_data("county", region = border_regions)

  # Return ggplot object with county and state boundary layers.
  ggplot(data = border_counties,
         mapping = aes(x = long, y = lat, group = group)) +
    geom_polygon(color = border_color, fill = border_fill,
                 size = border_size_county) +
    geom_polygon(data = border_states, fill = NA,
                 color = border_color, size = border_size_state) +
    theme(panel.grid = element_blank(), panel.background = element_blank(),
          panel.border = element_rect(colour = "slategrey", fill=NA, size=3))
}

# Build Specimen Layer ----

#' Build ggplot of specimen records over map borders.
#'
#' @param specimen_tbl Tibble of herbarium specimens subset.
#' @param id_column Character vector matching specimen tibble ID column.
#' @param shape_opt Character vector matching column for geom shape aesthetic.
#' @param geom_size Numeric scalar for size of jitter aesthetic.
#' @param jitter_pos Numeric vector of length two with jitter geom
#'   position arguments for width (1) and height (2). Defaults to `c(0, 0)`.
#' @param f_adj Numeric vector of length one for a fraction to extend
#'   the range of x and y limits.  Passed as limit arguments of
#' `grDevices::extendrange()` function call.
#' @inheritParams map_borders
#' @return Object of `ggplot` class with specimen data plotted over base layers
#'   of state and county borders.
#'
#' @examples
#' # Subset Colorado specimens and plot over state and county borders.
#' co_subset <- subset_coords(specimen_tbl = herbarium_specimens,
#'                            Longitude = c(-109, -105), Latitude = c(37, 41))
#' co_ggplot <- map_specimens(specimen_tbl = co_subset, id_column = "prior_id")
#'
map_specimens <- function(specimen_tbl, id_column, borders = "black",
                          shape_opt = NULL, geom_size = 3,
                          jitter_pos = c(0, 0), f_adj = -0.05, ...) {
  
  # Group specimens by count of identification column.
  id_quo <- enquo(id_column)
  specimen_tbl <- specimen_tbl %>%
    dplyr::group_by_at(vars(!!id_quo)) %>%
    dplyr::mutate(id_count = n()) %>% arrange(desc(id_count))

  # Plot specimens over state and county borders.
  map_gg_base <- map_borders(border_color = borders, ...)
  gg_basic_map <- map_gg_base +
    geom_jitter(data = specimen_tbl,
                mapping = aes_string(x = "Longitude", y = "Latitude",
                                     colour = id_column, shape = shape_opt),
                size = geom_size, inherit.aes = FALSE,
                width = jitter_pos[1], height = jitter_pos[2]) +
    coord_fixed(xlim = grDevices::extendrange(specimen_tbl$Longitude,
                                              f = f_adj),
                ylim = grDevices::extendrange(specimen_tbl$Latitude,
                                              f = f_adj)) +
    theme(panel.border = element_rect(colour = "slategrey", fill=NA, size=3)) +
    xlab("Longitude") +
    ylab("Latitude")

  # Return ggplot object with specimens mapped over `map_borders()` base layer.
  return(gg_basic_map)
}


# Build `ggmap` Layer ----

#' Build satellite and terrain maps for `ggplot` base layer.
#'
#' @param gg_borders `ggplot` object returned by `map_borders()` function.
#' @param size Numeric vector of length one for `ggmap::get_map()` zoom level.
#' @param gg_longitude Numeric vector of length one to optionally center call
#' of `get_map()` longitude coordinates.
#' @param gg_latitude Numeric vector of length one to optionally center call
#' of `get_map()` latitude coordinates.
#' @param gg_map_type Character vector of length one with type of map to return
#' from `ggmap::get_map()` function call.
#' @inheritParams map_filter
#' @inheritParams map_specimens
#'
#' @examples
#'
#' # Build county border map layer.
#' gg_borders <- map_borders("black")
#'
#' # Subset Colorado front range specimens.
#' co_front_range <- subset_coords(herbarium_specimens,
#'                                 Longitude = c(-107.7551, -104.2394),
#'                                 Latitude = c(38.12828, 40.84102))
#'
#' # Build ggmap object with borders and specimens plotted over satellite image.
#' co_ggmap <- map_ggmap(specimen_tbl = co_front_range, size = 8,
#'                       id_column = "Taxon_a_posteriori",
#'                       shape_opt = "Taxon_a_posteriori",
#'                       gg_longitude = -106, gg_latitude = 39.5,
#'                       gg_map_type = "satellite")
#'
#' # Add theme specifications and markdown legend.
#' map_themes(gg_map_obj = co_ggmap, mapped_specimens = co_front_range,
#'            id_column = "Taxon_a_posteriori")
#'
map_ggmap <- function(specimen_tbl, id_column, shape_opt = NULL,
                      size = 7, geom_size = 3,
                      border_size_county = .125,
                      border_size_state = 1.5,
                      jitter_pos = c(0, 0),
                      gg_longitude = NULL, gg_latitude = NULL,
                      gg_map_type = c("terrain", "satellite",
                                      "roadmap", "hybrid"), ...) {

  # Group specimens by count of identification column.
  id_quo <- enquo(id_column)
  specimen_tbl <- specimen_tbl %>%
    dplyr::group_by_at(vars(!!id_quo)) %>%
    dplyr::mutate(id_count = n()) %>% arrange(desc(id_count))

  # Check registration of Google API key.
  if (ggmap::has_google_key() == FALSE) {
    stop(paste("Register an API key with Google.",
               "See: https://github.com/dkahle/ggmap"))
  }

  # Filter specimens by coordinates and get ggmap by median values.
  if (!is.null(gg_longitude) && !is.null(gg_latitude)) {
    map_gg_sat <-
      ggmap::get_map(location = c(gg_longitude, gg_latitude), zoom = size,
                     maptype = gg_map_type, messaging = FALSE)
  } else if (!is.null(gg_longitude) || !is.null(gg_latitude)) {
    stop("Enter numeric vector for both longitude and latitude coordinates.")
  } else {
    gg_median <- sapply(specimen_tbl[, c("Longitude", "Latitude") ], median)
    map_gg_sat <-
      ggmap::get_map(location = gg_median, maptype = gg_map_type,
                     zoom = size, source = "google", messaging = FALSE)
  }

  # Index the boundary of the ggmap plot to the x/y limits of ggmap base.
  map_gg_base <- map_borders(border_color = "white", ...)
  map_bbox <- ggmap::bb2bbox(bb = attr(map_gg_sat, "bb"))
  map_xlim <- c(map_bbox["left"] * 0.9975, map_bbox["right"] * 1.0025)
  map_ylim <- c(map_bbox["bottom"] * 1.005, map_bbox["top"] * 0.995)

  # Plot Google base layer with county and state border geom layers.
  ggmap(ggmap = map_gg_sat, extent = "panel") +
    geom_path(data = map_gg_base$plot_env$border_counties,
              aes(x = long, y = lat, group = group),
              color = "white", size = .5) +
    geom_path(data = map_gg_base$plot_env$border_states,
              aes(x = long, y = lat, group = group),
              color = "moccasin", size = 1.25) +

    # Add specimen plot layer subset.
    geom_jitter(data = specimen_tbl,
                mapping = aes_string(x = "Longitude", y = "Latitude",
                                     colour = id_column, shape = shape_opt),
                size = geom_size, #inherit.aes = FALSE,
                width = jitter_pos[1], height = jitter_pos[2]) +

    # Adjust axis limits to edge of ggmap and modify theme.
    coord_map(projection = "mercator", xlim = map_xlim, ylim = map_ylim) +
    theme(panel.border = element_rect(colour = "black", fill=NA, size=3)) +
    scale_x_continuous("Longitude") +
    scale_y_continuous("Latitude")

}

# Build Elevation Raster Layer ----

#' Build elevation maps for `ggplot` base layer.
#'
#' @param raster_zoom Integer vector of length 1 for zoom level passed to
#' `elevatr::get_elev_raster()` function call.  Must be between 1 and 14.
#' @param raster_factor Integer vector of length 1 for optional aggregation
#' of elevation raster to decrease resolution / object size.
#' @inheritParams map_filter
#' @inheritParams map_specimens
#' @inheritParams map_ggmap
#'
#' @examples
#' co_elev <- map_elev(specimen_tbl = co_front_range, raster_zoom = 7,
#'                     id_column = "Taxon_a_posteriori",
#'                     shape_opt = "Taxon_a_posteriori")
#' 
#' # Add theme specifications and markdown legend.
#' map_themes(gg_map_obj = co_elev, mapped_specimens = co_front_range,
#'            id_column = "Taxon_a_posteriori")
#'
map_elev <- function(specimen_tbl, id_column, shape_opt = NULL,
                     raster_zoom = 7, raster_factor = 2, geom_size = 3, ...) {

  # Group specimens by count of identification column.
  id_quo <- enquo(id_column)
  specimen_tbl <- specimen_tbl %>%
    dplyr::group_by_at(vars(!!id_quo)) %>%
    dplyr::mutate(id_count = n()) %>% arrange(desc(id_count))

  # Plot specimens over state and county borders.
  map_gg_base <- map_borders(border_color = "black", ...)

  # Define projection and get AWS Open Data terrain tiles.
  # Cite: https://registry.opendata.aws/terrain-tiles/
  prj_dd <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  map_elev_raster <-
    get_elev_raster(locations =
                      as.data.frame(specimen_tbl[, c("Longitude", "Latitude")]),
                    z = raster_zoom, prj = prj_dd, clip = "bbox", src = "aws")

  # Use "raster" package function aggregate() to decrease data resolution and
  # reduce the size / memory of elevation raster objects (Hijmans 2017).
  # Cite: http://pakillo.github.io/R-GIS-tutorial/#resolution
  if (raster_factor > 1) {
    map_elev_raster <-
      raster::aggregate(map_elev_raster, fact = raster_factor, fun = mean)
  }

  # Use "sp" package to define a spatial grid from the raster layer.
  # Cite: https://groups.google.com/forum/#!msg/ggplot2/9fS4OfHEQq8/ZafTyvVKfJIJ
  map_elev_df <-
    as(map_elev_raster, "SpatialPixelsDataFrame") %>% as.data.frame()

  # ggplot elevation projection with county & state borders.
  map_elev_ggplot <- ggplot(map_elev_df, aes(x = x, y = y)) +
    geom_tile(aes(fill = layer)) +
    geom_polygon(data = map_gg_base$plot_env$border_counties,
                 aes(x = long, y = lat, group = group),
                 color = "black", fill = NA, size = .5) +
    geom_polygon(data = map_gg_base$plot_env$border_states,
                 aes(x = long, y = lat, group = group),
                 color = "moccasin", fill = NA, size = 1.25) +

    # Add specimen data layers and modify theme elements.
    geom_point(data = specimen_tbl, aes(x = Longitude, y = Latitude),
               size = (geom_size + 2), colour = "black", alpha = 0.2) +
    geom_point(data = specimen_tbl, size = geom_size, na.rm = TRUE,
               aes_string(x = "Longitude", y = "Latitude",
                   colour = id_column, shape = shape_opt)) +
    scale_x_continuous("Longitude") +
    scale_y_continuous("Latitude") +
    coord_equal(xlim = c(min(specimen_tbl$Longitude),
                         max(specimen_tbl$Longitude)),
                ylim = c(min(specimen_tbl$Latitude),
                         max(specimen_tbl$Latitude)),
                expand = FALSE) +
    scale_fill_gradientn("Elevation (m)", colours = terrain.colors(7),
                         guide = guide_colourbar(order = 1)) +
    theme(panel.grid = element_blank(), panel.background = element_blank(),
          legend.direction = "vertical", legend.position = "bottom",
          panel.border = element_rect(colour = "slategrey", fill=NA, size=3))
}

# Specimen Annotation ----

#' Map Individual Specimen
#'
#' @param gg_map_obj ggplot object returned by `map_specimens()` or
#' `map_ggmap()` functions.`
#' @param h_adjust Numeric vector of length one for horizontal label adjustment.
#' @param v_adjust Numeric vector of length one for vertical label adjustment.
#' @param label_size Numeric vector of length one for label size.
#' @inheritParams find_spp
#'
#' @return ggplot object with added specimen annotation layer.
#'
map_spp_id <- function(gg_map_obj, specimen_tbl, collector, collection,
                       id_column, shape_opt = NULL, geom_size = 3,
                       h_adjust = 0.25, v_adjust = -0.15, label_size = 3) {

  # Call `find_spp()` function to get specimen annotation data.
  spp_id <- find_spp(specimen_tbl = specimen_tbl,
                     collector = collector, collection = collection) %>%
    dplyr::mutate(taxa_label = stringr::str_remove_all(string = Collector,
       pattern = "[A-Z]\\. ?") %>% stringr::str_replace(string = .,
       pattern = "and|with", replacement =  "&") %>%
    paste(., Collection_Number, collapse = "", sep = "\n"))

  # Plot additional map layer to include the specimen returned by spp_find().
  gg_map_obj +
    geom_point(data = spp_id, inherit.aes = FALSE,
               mapping = aes(x = Longitude, y = Latitude),
               show.legend = FALSE, size = 5, shape = 5, fill = NA) +
    geom_point(data = spp_id, size = geom_size,
               mapping = aes_string(x = "Longitude", y = "Latitude",
                                    colour = id_column, shape = shape_opt)) +
    geom_label(data = spp_id, inherit.aes = FALSE,
               nudge_x = h_adjust, nudge_y = v_adjust,
               label.padding = unit(0.1, "lines"), size = label_size,
               mapping = aes(x = Longitude, y = Latitude,
                             label = taxa_label), alpha = 0.5)

}

# Map Themes ----

spp_color <- c("Physaria acutifolia" = "yellow",
               "Physaria vitulifera" = "plum",
               "Physaria medicinae" = "purple2",
               "Physaria acutifolia - vitulifera-like" = "turquoise",
               "Physaria vitulifera - Carbon" = "steelblue",
               "Physaria floribunda" = "gold",
               "Physaria floribunda ssp. floribunda" = "goldenrod",
               "Physaria floribunda ssp. osterhoutii" = "limegreen",
               "Physaria bellii" = "blue",
               "Physaria rollinsii" = "darkorchid1",
               "Physaria alpina" = "firebrick",
               "Physaria geyeri" = "plum1",
               "Physaria brassicoides" = "olivedrab2",
               "Physaria didymocarpa ssp. didymocarpa" = "skyblue",
               "Physaria didymocarpa ssp. lanata" = "goldenrod",
               "Physaria didymocarpa ssp. lyrata"= "black",
               "Physaria saximontana ssp. dentata" = "maroon",
               "Physaria didymocarpa ssp. saximontana" = "thistle4",
               "Physaria saximontana ssp. saximontana" = "indianred2",
               "Physaria eburniflora" = "cyan",
               "Physaria integrifolia" =  "seagreen",
               "Physaria dornii" = "darkorange",
               "Physaria condensata"= "mediumblue",
               "Physaria chambersii" = "springgreen",
               "Physaria" = "seashell",
               "Physaria flowering" = "seashell",
               "Physaria obcordata" = "black",
               "Physaria fendleri" = "black",
               "Physaria argyraea" = "black")

spp_shape <- c("Physaria acutifolia" = 3,
               "Physaria vitulifera"= 8,
               "Physaria medicinae" = 17,
               "Physaria acutifolia - vitulifera-like" = 17,
               "Physaria vitulifera - Carbon" = 17,
               "Physaria floribunda" = 15,
               "Physaria floribunda ssp. floribunda" = 18,
               "Physaria floribunda ssp. osterhoutii" = 18,
               "Physaria bellii" = 4,
               "Physaria rollinsii" = 17,
               "Physaria alpina" = 15,
               "Physaria geyeri" = 18,
               "Physaria brassicoides" = 16,
               "Physaria didymocarpa ssp. didymocarpa" = 15,
               "Physaria didymocarpa ssp. lanata" = 25,
               "Physaria didymocarpa ssp. lyrata" = 17,
               "Physaria saximontana ssp. dentata" = 18,
               "Physaria didymocarpa ssp. saximontana" = 18,
               "Physaria saximontana ssp. saximontana" = 17,
               "Physaria eburniflora" = 18,
               "Physaria integrifolia" = 18,
               "Physaria dornii" = 16,
               "Physaria condensata"= 16,
               "Physaria chambersii" = 17,
               "Physaria" = 16,
               "Physaria flowering" = 16,
               "Physaria obcordata" = 16,
               "Physaria fendleri" = 15,
               "Physaria argyraea" = 18)

#' Modify map ggplot themes.
#'
#' This function overwrites the default discrete scales present in map ggplots
#' with manual values set by `spp_color` and `spp_shape` vectors.
#'
#' @param gg_map_obj Ggmap returned by specimen mapping function.
#' @param mapped_specimens Tibble of specimens to map.
#'  Use same argument for `map_specimens(specimen_tbl = [mapped_specimens])`
#' @param id_column Character vector matching specimen tibble ID column.
#' @param legend_title Character vector of length one to set the ggplot
#' legend title.
#'
#' @examples
#' map_themes(gg_map_obj = co_ggplot, mapped_specimens = co_subset,
#'            id_column = "prior_id", legend_title = "Prior Annotations")
#'
#' map_themes(gg_map_obj = co_ggmap, mapped_specimens = co_front_range,
#'            id_column = "Taxon_a_posteriori")
map_themes <- function(gg_map_obj, mapped_specimens, id_column,
                       legend_title = "Reviewed Annotations") {

  # Assign map specimen tibble cast from data frame
  if (!tibble::is_tibble(mapped_specimens)) {
    mapped_specimens <- tibble::as_tibble(mapped_specimens)
  }

  # Reset discrete scales to manually set shape and colour scales.
  scale_index <-
    grep(pattern = "ScaleDiscrete",
         x = lapply(gg_map_obj$scales$scales, class), invert = TRUE)
  gg_map_obj$scales$scales <- gg_map_obj$scales$scales[scale_index]

  # Build ggplot map object with manual scales.
  gg_map_obj +
    scale_color_manual(name = legend_title,
                       labels = spp_labels(specimen_tibble = mapped_specimens,
                                           id_column = id_column),
                       values = spp_color, na.value = "black") +

    scale_shape_manual(name = legend_title,
                       labels = spp_labels(specimen_tibble = mapped_specimens,
                                           id_column = id_column),
                       values = spp_shape, na.value = 17) +

    theme(legend.text.align = 0, legend.title.align = 0.5,
          legend.direction = "vertical", legend.key= element_blank(),
          legend.background = element_rect(fill = "grey90",
                                           color =  "black"),
          legend.text = ggtext::element_markdown())

}

