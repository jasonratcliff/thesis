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
#' @inheritParams map_borders
#' @inheritParams map_specimens
#'
#' @examples
#'
#' # Build county border map layer.
#' gg_borders <- map_borders("black")
#'
#' # Subset Colorado front range specimens.
#' co_front_range <-
#'   subset_spp(total_physaria,
#'              longitude = c(-107.7551, -104.2394),
#'              latitude = c(38.12828, 40.84102),
#'              spp_str = "Physaria \\?",
#'              taxa_col = "Taxon_a_posteriori", exclude = TRUE)
#'
#' # Build ggmap object with borders and specimens plotted over satellite image.
#' co_ggmap <- map_ggmap(map_df = co_front_range,
#'                       map_col = "Taxon_a_posteriori",
#'                       gg_borders = gg_borders, size = 8,
#'                       gg_longitude = -106, gg_latitude = 39.5,
#'                       gg_map_type = "satellite")
#'
map_ggmap <- function(map_df, map_col, gg_borders, size = 7,
                      border_size_county = .125,
                      border_size_state = 1.5,
                      shape_opt = NULL, geom_size = 3,
                      jitter_pos = c(0, 0),
                      gg_longitude = NULL, gg_latitude = NULL,
                      gg_map_type = c("terrain", "satellite",
                                     "roadmap", "hybrid")) {

  # Check registration of Google API key.
  if (has_google_key() == FALSE) {
    stop(paste("Register an API key with Google.",
               "See: https://github.com/dkahle/ggmap"))
  }

  # Filter specimens by coordinates and get ggmap by median values.
  map_subset <- map_filter(map_df)
  if (!is.null(gg_longitude) && !is.null(gg_latitude)) {
    map_gg_sat <- get_map(location = c(gg_longitude, gg_latitude), zoom = size,
                          maptype = gg_map_type, messaging = FALSE)
  } else if (!is.null(gg_longitude) || !is.null(gg_latitude)) {
    stop("Enter numeric vector for both longitude and latitude coordinates.")
  } else {
    gg_median <- sapply(map_subset[, c("Longitude", "Latitude") ], median)
    map_gg_sat <- get_map(location = gg_median, maptype = gg_map_type,
                          zoom = size, source = "google", messaging = FALSE)
  }

  # Index the boundary of the ggmap plot to the x/y limits of ggmap base.
  map_xlim = c(attr(map_gg_sat, "bb")$ll.lon,
               attr(map_gg_sat, "bb")$ur.lon)
  map_ylim = c(attr(map_gg_sat, "bb")$ll.lat,
               attr(map_gg_sat, "bb")$ur.lat)

  # Plot Google base layer with county and state border geom layers.
  gg_sat_map <- ggmap(ggmap = map_gg_sat,
                      extent = "normal", maprange = FALSE) +
    geom_polygon(data = gg_borders$plot_env$border_counties,
                 aes(x = long, y = lat, group = group),
                 color = "white", fill = NA, size = .5) +
    geom_polygon(data = gg_borders$plot_env$border_states,
                 aes(x = long, y = lat, group = group),
                 color = "moccasin", fill = NA, size = 1.25) +

    # Add specimen plot layer subset.
    geom_jitter(data = map_subset,
                mapping = aes_string(x = "Longitude", y = "Latitude",
                                     colour = map_col, shape = shape_opt),
                size = geom_size, inherit.aes = FALSE,
                width = jitter_pos[1], height = jitter_pos[2]) +

    # Adjust axis limits to edge of ggmap
    coord_map(projection = "mercator", xlim = map_xlim, ylim = map_ylim) +
    theme(panel.border = element_rect(colour = "slategrey", fill=NA, size=3)) +
    xlab("Longitude") +
    ylab("Latitude")

  # Return ggplot of specimens mapped over ggmap and county border layers.
  return(gg_sat_map)
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
map_spp_id <- function(gg_map_obj, taxa_frame, collector, collection_number,
                       h_adjust = 0.25, v_adjust = -0.15, label_size = 3) {

  # Call `find_spp()` function to get specimen annotation data.
  spp_id <- find_spp(taxa_frame = taxa_frame,
                     collector = collector,
                     collection_number = collection_number,
                     geom = TRUE, label = TRUE)

  # Plot additional map layer to include the specimen returned by find_spp().
  gg_spp_id <- gg_map_obj +
      geom_point(data = spp_id, inherit.aes = FALSE,
                 mapping = aes(x = Longitude, y = Latitude),
                 size = 5, shape = 23, colour = "black", fill = "white") +
      geom_label(data = spp_id, inherit.aes = FALSE,
                 nudge_x = h_adjust, nudge_y = v_adjust,
                 label.padding = unit(0.1, "lines"), size = label_size,
                 mapping = aes(x = Longitude, y = Latitude,
                               label = taxon_label), alpha = 0.5)

  # Return ggplot map with specimen annotation.
  return(gg_spp_id)
}


