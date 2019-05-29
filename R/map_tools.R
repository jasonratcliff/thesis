# Specimen Mapping Tools ----

#' Filter specimens by coordinate notation and specimen observations.
#'
#' @param map_df Data frame of herbarium specimens to map.
#' @param map_geom Logical vector of length one to subset records with
#' coordinates formatted as decimal degrees.
#' @param map_obs_col Character vector of length one matching column name to
#' subset specimen observations by `map_obs` pattern.
#' @param map_obs Character vector of length one with the observation type by
#' which to subset specimen records from the `map_obs_col` column.
#' @return Data frame subset of specimen observations.
#'
#' @examples
#' wyo_eburniflora <- subset_spp(taxa_frame = total_physaria,
#'                               state = "Wyoming",
#'                               spp_str = "eburniflora",
#'                               taxa_col = "Taxon")
#'
#' wyo_ebur_remaining <- map_filter(map_df = wyo_eburniflora,
#'                                  map_obs_col = "Physaria_syn",
#'                                  map_obs = "questioned")
#'
#' wyo_ebur_remaining[, c("Physaria_syn", "Collector", "Collection_Number")]
#' #               Physaria_syn Collector Collection_Number
#' # 120 Physaria eburniflora ? J. Haines              4792
#'
#' wyo_ebur_remaining[, c("Longitude", "Latitude")]
#' #     Longitude Latitude
#' # 120 -107.4294  43.2923
#'
map_filter <- function(map_df, map_geom = TRUE, map_obs_col = NULL,
                       map_obs = c("confirmed", "questioned", "remaining")) {

  # Check if `map_df` input is a data frame with coordinate vectors.
  if (!is.data.frame(map_df) ||
      (!"Longitude" %in% names(map_df) || !"Latitude" %in% names(map_df))) {
        stop("Input data frame must have longitude / latitude vectors.")
  } else {
    map_subset <- map_df   # Initialize data frame for subset assignment.
  }

  # Filter `map_subset` data frame to rows with degree decimal coordinates.
  if (map_geom == TRUE) {
    map_subset <- subset(map_subset,
                         grepl("^-?[0-9]+\\.?[0-9]+?", map_subset$Longitude) &
                           grepl("[0-9]+\\.?[0-9]+", map_subset$Latitude))
  }

  # Filter specimens by observation type in respective taxa column.
  if (length(map_obs) == 1 && !is.null(map_obs_col)) {
    if(map_obs == "questioned") {
      map_subset <- map_subset[grep("\\?", map_subset[, map_obs_col]), ]
    }
    if(map_obs == "remaining") {
      map_subset <- map_subset[is.na(map_subset[, map_obs_col]), ]
    }
    if(map_obs == "confirmed") {
      map_subset <-
        map_subset[grep("\\?", map_subset[, map_obs_col], invert = TRUE), ]
    }
  } else if (length(map_obs) == 1 && is.null(map_obs_col) ||
             length(map_obs) != 1 && !is.null(map_obs_col)) {
    stop("Enter a single argument for `map_obs` with respective taxa column.")
  }

  # Return specimen subset for mapping.
  return(map_subset)
}

# Build Border Layer ----

#' Build ggplot layer of state and county borders.
#'
#' Utilize US Department of Commerce Census Bureau data to plot state and county
#' boundaries using the `ggplot2::map_data()` extension of the `maps` package.
#'
#' @param border_color Text value for `geom_polygon` color aesthetic mapping.
#' @param border_fill Text value for `geom_polygon` fill aesthetic mapping.
#' Default is set to `NA` for a transparent fill of county polygons.
#' @param border_size_county Numeric value for county border size aesthetic.
#' @param border_size_state Numeric value for state border size aesthetic.
#' @param border_regions Character vector of states to query `maps` package
#' for county and state boundary files.
#' @return Object of `ggplot` class with polygon layers of state and county
#' borders.
map_borders <- function(border_color, border_fill = NA,
                        border_size_county = .125,
                        border_size_state = 1.5,
                        border_regions = c("Montana", "Wyoming", "Colorado",
                                           "Utah", "Idaho", "Nebraska",
                                           "North Dakota", "South Dakota")) {

  # Initialize ggplot base map layer of county lines with state borders.
  border_states <- map_data("state", region = border_regions)
  border_counties <- map_data("county", region = border_regions)
  gg_borders <-
    ggplot(data = border_counties,
           mapping = aes(x = long, y = lat, group = group)) +
    geom_polygon(color = border_color, fill = border_fill,
                 size = border_size_county) +
    geom_polygon(data = border_states, fill = NA,
                 color = border_color, size = border_size_state) +
    theme(panel.grid = element_blank(), panel.background = element_blank(),
          panel.border = element_rect(colour = "slategrey", fill=NA, size=3))

  # Return ggplot object with county and state boundary layers.
  return(gg_borders)
}

# Build Specimen Layer ----

#' Build ggplot of specimen records over map borders.
#'
#' @param map_col Character vector of length one with geom color aesthetic.
#' @param map_gg_base Border map `ggplot` output by `map_borders()` function.
#' @param shape_opt Character vector of length one with column for shape
#' aesthetic. Null as default.
#' @param geom_size Numeric vector of length one for size of specimen plot
#' jitter aesthetic.
#' @param jitter_pos Numeric vector of length two with jitter geom
#' position arguments for width (1) and height (2). Defaults to `c(0, 0)`.
#' @param f_adj Numeric vector of length one for a fraction to extend
#' the range of x and y limits.  Passed as limtit arguments of 
#' `grDevices::extendrange()` function call.
#' @inheritParams map_filter
#' @return Object of `ggplot` class with specimen data plotted over base layers
#' of state and county borders.
#'
#' @examples
#' # Subset Colorado specimens
#' co_subset <- subset_spp(taxa_frame = total_physaria,
#'                         state = "Colorado",
#'                         longitude = c(-109, -105), latitude = c(37, 41))
#' 
#' # Plot specimens over state and county border ggplot
#' co_ggplot <- map_specimens(map_df = co_subset, map_col = "Physaria_syn")
#' 
map_specimens <- function(map_df, map_col, map_gg_base = NULL,
                          shape_opt = NULL, geom_size = 3,
                          jitter_pos = c(0, 0), f_adj = -0.05) {

  # Subset data to ensure coordinates are formatted in degree decimal notation.
  map_subset <- map_filter(map_df = map_df, map_geom = TRUE)

  # Plot specimens over state and county borders.
  if (is.null(map_gg_base)) {
    map_gg_base <- map_borders(border_color = "black")
  }
  gg_basic_map <- map_gg_base +
    geom_jitter(data = map_subset,
                mapping = aes_string(x = "Longitude", y = "Latitude",
                                     colour = map_col, shape = shape_opt),
                size = geom_size, inherit.aes = FALSE,
                width = jitter_pos[1], height = jitter_pos[2]) +
    coord_fixed(xlim = grDevices::extendrange(map_subset$Longitude,
                                              f = f_adj),
                ylim = grDevices::extendrange(map_subset$Latitude,
                                              f = f_adj)) +
    theme(panel.border = element_rect(colour = "slategrey", fill=NA, size=3)) +
    xlab("Longitude") +
    ylab("Latitude")

  # Return ggplot object with specimens mapped over `map_borders()` base layer.
  return(gg_basic_map)
}

