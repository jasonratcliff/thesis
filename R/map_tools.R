
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
#' wyo_eburniflora <- spp_subset(taxa_frame = total_physaria,
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
#' co_subset <- spp_subset(taxa_frame = total_physaria,
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
#'   spp_subset(total_physaria,
#'              longitude = c(-107.7551, -104.2394),
#'              latitude = c(38.12828, 40.84102),
#'              spp_str = "Physaria \\?",
#'              taxa_col = "Taxon_a_posteriori", exclude = TRUE)
#'
#' # Build ggmap object with borders and specimens plotted over satellite image.
#' co_ggmap <- map_ggmap(map_df = co_front_range,
#'                       map_col = "Taxon_a_posteriori",
#'                       shape_opt = "Taxon_a_posteriori",
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
  gg_sat_map <-
    ggmap(ggmap = map_gg_sat, extent = "normal", maprange = FALSE) +
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

    # Adjust axis limits to edge of ggmap and modify theme.
    coord_map(projection = "mercator", xlim = map_xlim, ylim = map_ylim) +
    theme(panel.border = element_rect(colour = "slategrey", fill=NA, size=3)) +
    scale_x_continuous("Longitude") +
    scale_y_continuous("Latitude")

  # Return ggplot of specimens mapped over ggmap and county border layers.
  return(gg_sat_map)
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
#' co_elev <- elev_spp(specimens = co_front_range, raster_zoom = 7)
#'
map_elev <- function(map_df, map_col, gg_borders,
                     raster_zoom = 7, raster_factor = 2, geom_size = 3) {

  # Define projection and get AWS Open Data terrain tiles.
  # Cite: https://registry.opendata.aws/terrain-tiles/
  prj_dd <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  map_elev_raster <-
    get_elev_raster(locations = map_df[, c("Longitude", "Latitude")],
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
  map_elev_ggplot <- ggplot(map_elev_df, aes(x=x, y=y)) +
    geom_tile(aes(fill = layer)) +
    geom_polygon(data = gg_borders$plot_env$border_counties,
                 aes(x = long, y = lat, group = group),
                 color = "black", fill = NA, size = .5) +
    geom_polygon(data = gg_borders$plot_env$border_states,
                 aes(x = long, y = lat, group = group),
                 color = "moccasin", fill = NA, size = 1.25) +

    # Add specimen data layers and modify theme elements.
    geom_point(data = map_df, aes(x = Longitude, y = Latitude),
               size = (geom_size + 2), colour = "black", alpha = 0.2) +
    geom_point(data = map_df, size = geom_size, na.rm = TRUE,
               aes(x = Longitude, y = Latitude, 
                   colour = get(map_col), shape = get(map_col))) +
    scale_x_continuous("Longitude") +
    scale_y_continuous("Latitude") +
    coord_equal(xlim = c(min(map_df$Longitude), max(map_df$Longitude)),
                ylim = c(min(map_df$Latitude), max(map_df$Latitude)),
                expand = FALSE) +
    scale_fill_gradientn("Elevation (m)", colours = terrain.colors(7)) +
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
#' @inheritParams spp_find
#'
#' @return ggplot object with added specimen annotation layer.
#'
map_spp_id <- function(gg_map_obj, taxa_frame, collector, collection_number,
                       h_adjust = 0.25, v_adjust = -0.15, label_size = 3) {

  # Call `spp_find()` function to get specimen annotation data.
  spp_id <- spp_find(taxa_frame = taxa_frame,
                     collector = collector,
                     collection_number = collection_number,
                     geom = TRUE, label = TRUE)

  # Plot additional map layer to include the specimen returned by spp_find().
  gg_spp_id <- gg_map_obj +
      geom_point(data = spp_id, inherit.aes = FALSE,
                 mapping = aes(x = Longitude, y = Latitude),
                 show.legend = FALSE, size = 5, shape = 5, fill = NA) +
      geom_label(data = spp_id, inherit.aes = FALSE,
                 nudge_x = h_adjust, nudge_y = v_adjust,
                 label.padding = unit(0.1, "lines"), size = label_size,
                 mapping = aes(x = Longitude, y = Latitude,
                               label = taxon_label), alpha = 0.5)

  # Return ggplot map with specimen annotation.
  return(gg_spp_id)
}

# Aesthetic Vectors for `ggplot`----

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
               "Lesquerella fendleri" = "black",
               "Lesquerella argyrea" = "black")

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
               "Lesquerella fendleri" = 15,
               "Lesquerella argyrea" = 18)

spp_labels <- c("Physaria acutifolia" =
                  expression(italic("Physaria acutifolia")),
                "Physaria vitulifera"=
                  expression(italic("Physaria vitulifera")),
                "Physaria medicinae" =
                  expression(italic("Physaria medicinae")),
                "Physaria acutifolia - vitulifera-like" =
                  expression(italic("Physaria acutifolia")*" - "*italic("vitulifera")*"-like"),
                "Physaria vitulifera - Carbon" =
                  expression(italic("Physaria vitulifera")*" - Carbon"),
                "Physaria floribunda" =
                  expression(italic("Physaria floribunda")),
                "Physaria floribunda ssp. floribunda" =
                  expression(italic("Physaria floribunda")*" ssp. "*italic("floribunda")),
                "Physaria floribunda ssp. osterhoutii" =
                  expression(italic("Physaria floribunda")*" ssp. "*italic("osterhoutii")),
                "Physaria bellii" = 
                  expression(italic("Physaria bellii")),
                "Physaria rollinsii" = 
                  expression(italic("Physaria rollinsii")),
                "Physaria alpina" =
                  expression(italic("Physaria alpina")),
                "Physaria brassicoides" =
                  expression(italic("Physaria brassicoides")),
                "Physaria didymocarpa ssp. didymocarpa" =
                  expression(italic("Physaria didymocarpa")*" ssp. "*italic("didymocarpa")),
                "Physaria didymocarpa ssp. lanata" =
                  expression(italic("Physaria didymocarpa")*" ssp. "*italic("lanata")),
                "Physaria didymocarpa ssp. lyrata" =
                  expression(italic("Physaria didymocarpa")*" ssp. "*italic("lyrata")),
                "Physaria saximontana ssp. dentata" =
                  expression(italic("Physaria saximontana")*" ssp. "*italic("dentata")),
                "Physaria didymocarpa ssp. saximontana" =
                  expression(italic("Physaria didymocarpa")*" ssp. "*italic("saximontana")),
                "Physaria saximontana ssp. saximontana" =
                  expression(italic("Physaria saximontana")*" ssp. "*italic("saximontana")),
                "Physaria eburniflora" =
                  expression(italic("Physaria eburniflora")),
                "Physaria integrifolia" =
                  expression(italic("Physaria integrifolia")),
                "Physaria dornii" = 
                  expression(italic("Physaria dornii")),
                "Physaria condensata" =
                  expression(italic("Physaria condensata")),
                "Physaria chambersii" =
                  expression(italic("Physaria chambersii")),
                "Physaria" =
                  expression(italic("Physaria")),
                "Physaria flowering" =
                  expression(italic("Physaria flowering")),
                "Lesquerella fendleri" =
                  expression(italic("Lesquerella fendleri")),
                "Lesquerella argyrea" =
                  expression(italic("Lesquerella argyrea")))

#' Modify map ggplot themes.
#'
#' This function overwrites the default discrete scales present in map ggplots
#' with manual values set by `spp_color` and `spp_shape` vectors.
#'
#' @param legend_title Character vector of length one to set the ggplot
#' legend title.
#' @inheritParams map_spp_id
#'
#' @examples
#' map_themes(gg_map_obj = co_ggmap)
#'
map_themes <- function(gg_map_obj, legend_title = "Reviewed Annotations") {

  # Reset discrete scales to manually set shape and colour scales.
  scale_index <-
    grep(pattern = "ScaleDiscrete",
         x = lapply(gg_map_obj$scales$scales, class), invert = TRUE)
  gg_map_obj$scales$scales <- gg_map_obj$scales$scales[scale_index]

  # Build ggplot map object with manual scales.
  gg_map_obj +
    scale_color_manual(name = legend_title, labels = spp_labels,
                       values = spp_color, na.value = "black") +
    scale_shape_manual(name = legend_title, labels = spp_labels,
                       values = spp_shape, na.value = 17) +
    theme(legend.text.align = 0, legend.title.align = 0.5)
  
}

