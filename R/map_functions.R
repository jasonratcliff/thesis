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
#' @import ggplot2
#'
#' @return Object of `ggplot` class with polygon layers of state and county
#' borders.
#'
#' @examples
#' map_gg_base <- map_borders(border_color = "black")
#'
#' @export
#'
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
         mapping = aes(x = .data$long, y = .data$lat, group = .data$group)) +
    geom_polygon(color = border_color, fill = border_fill,
                 size = border_size_county) +
    geom_polygon(data = border_states, fill = NA,
                 color = border_color, size = border_size_state) +
    theme(panel.grid = element_blank(),
          panel.background = element_blank(),
          panel.border = element_rect(colour = "slategrey", fill=NA, size=3))
}

# Build Specimen Layer ----

#' Build ggplot of specimen records over map borders.
#'
#' @param specimen_tbl Tibble of herbarium specimens subset.
#' @param id_column Character scalar matching specimen tibble ID column.
#' @param borders Charcacter scalar for color of ggplot borders.
#' @param shape_opt Character scalar matching column for geom shape aesthetic.
#' @param geom_size Numeric scalar for size of jitter aesthetic.
#' @param jitter_pos Numeric vector of length two with jitter geom
#'   position arguments for width (1) and height (2). Defaults to `c(0, 0)`.
#' @param f_adj Numeric vector of length one for a fraction to extend
#'   the range of x and y limits.  Passed as limit arguments of
#' `grDevices::extendrange()` function call.
#' @param ... Options inherited for `map_borders()` function call.
#' @import ggplot2
#' @importFrom rlang !!
#'
#' @return Object of `ggplot` class with specimen data plotted over base layers
#'   of state and county borders.
#'
#' @examples
#' # Subset Colorado specimens and plot over state and county borders.
#' co_subset <- subset_coords(specimen_tbl = herbarium_specimens,
#'                            Longitude = c(-109, -105), Latitude = c(37, 41))
#' co_ggplot <- map_specimens(specimen_tbl = co_subset, id_column = "prior_id")
#'
#' @export
#'
map_specimens <- function(specimen_tbl, id_column, borders = "black",
                          shape_opt = NULL, geom_size = 3,
                          jitter_pos = c(0, 0), f_adj = -0.05, ...) {

  # Group specimens by count of identification column.
  id_quo <- rlang::enquo(id_column)
  specimen_tbl <- specimen_tbl %>%
    dplyr::group_by_at(vars(!!id_quo)) %>%
    dplyr::mutate(id_count = dplyr::n()) %>%
    dplyr::arrange(dplyr::desc(.data$id_count))

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
#' @param size Numeric vector of length one for `ggmap::get_map()` zoom level.
#' @param gg_longitude Numeric vector of length one to optionally center call
#' of `get_map()` longitude coordinates.
#' @param gg_latitude Numeric vector of length one to optionally center call
#' of `get_map()` latitude coordinates.
#' @param gg_map_type Character vector of length one with type of map to return
#' from `ggmap::get_map()` function call.
#' @inheritParams map_specimens
#' @importFrom rlang .data !!
#' @import ggplot2
#'
#' @examples
#' # Subset Colorado front range specimens.
#' co_front_range <- subset_coords(herbarium_specimens,
#'                                 Longitude = c(-107.7551, -104.2394),
#'                                 Latitude = c(38.12828, 40.84102))
#'
#' # Build ggmap object with borders and specimens plotted over satellite image.
#' co_ggmap <- map_ggmap(specimen_tbl = co_front_range, size = 8,
#'                       id_column = "Taxon_a_posteriori",
#'                       shape_opt = "Taxon_a_posteriori",
#'                       #gg_longitude = -106, gg_latitude = 39.5,
#'                       gg_map_type = "satellite")
#'
#' @export
#'
map_ggmap <- function(specimen_tbl, id_column, shape_opt = NULL,
                      size = 7, geom_size = 3, jitter_pos = c(0, 0),
                      gg_longitude = NULL, gg_latitude = NULL,
                      gg_map_type = c("terrain", "satellite",
                                      "roadmap", "hybrid"), ...) {

  # Group specimens by count of identification column.
  id_quo <- rlang::enquo(id_column)
  specimen_tbl <- specimen_tbl %>%
    dplyr::group_by_at(vars(!!id_quo)) %>%
    dplyr::mutate(id_count = dplyr::n()) %>%
    dplyr::arrange(dplyr::desc(.data$id_count))

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
    gg_median <- sapply(specimen_tbl[, c("Longitude", "Latitude") ],
                        stats::median)
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
  ggmap::ggmap(ggmap = map_gg_sat, extent = "panel") +
    geom_path(data = map_gg_base$plot_env$border_counties,
              aes(x = .data$long, y = .data$lat, group = .data$group),
              color = "white", size = .5) +
    geom_path(data = map_gg_base$plot_env$border_states,
              aes(x = .data$long, y = .data$lat, group = .data$group),
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

# Map Themes ----

#' Modify map ggplot themes.
#'
#' This function overwrites the default discrete scales present in map ggplots
#' with manual values set by `spp_color` and `spp_shape` vectors.
#'
#' @param gg_map_obj Ggmap returned by specimen mapping function.
#' @param mapped_specimens Tibble of specimens to map.
#'   Use same argument for `map_specimens(specimen_tbl = [mapped_specimens])`
#' @param legend_title Character vector of length one to set the ggplot
#'   legend title.
#' @import ggplot2
#' @inheritParams map_specimens
#' @inherit map_ggmap examples
#'
#' @examples
#'
#' # Add theme specifications and markdown legend.
#' \dontrun{
#' map_themes(gg_map_obj = co_ggmap, mapped_specimens = co_front_range,
#'            id_column = "Taxon_a_posteriori")
#' }
#'
#' @export
#'
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

