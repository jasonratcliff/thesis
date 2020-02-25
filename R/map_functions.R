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

