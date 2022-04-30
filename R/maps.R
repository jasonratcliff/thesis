# Specimen Mapping ------------------------------------------------------------

#' @title Specimen Mapping
#'
#' @description
#' This R6 subclass inherits the [Thesis::Specimen] superclass,
#' providing public methods to plot records by geographic coordinate.
#' Boundary shapefiles are downloaded from the US Census Bureau
#' Topologically Integrated Geographic Encoding and Referencing (TIGER)
#' database using [tigris](https://github.com/walkerke/tigris)
#' (Walker 2016). Data are read in to R as simple features with the
#' [sf](https://r-spatial.github.io/sf/) package (Pebesma 2018) and
#' transformed by [rmapshaper](https://github.com/ateucher/rmapshaper)
#' (Teucher and Russell 2020) to simplify border polygons via the
#' Visvalingam algorithm.
#'
#' @details
#'
#' * **`ggmap`**
#'
#' The \href{#method-map}{\code{SpecimenMap$map()}}
#' method exposes an option to include satellite or
#' terrain rasters image from [ggmap::get_map()] as a baselayer to the specimen
#' distribution map. Access to the static maps requires API key registration.
#' Verify a key is set to the environment variable `GGMAP_GOOGLE_API_KEY`
#' using [ggmap::has_google_key()]. For options to register a key, see
#' [ggmap::register_google()] – *ensure private keys are kept confidential*.
#' See the `README` at <https://github.com/dkahle/ggmap> for more info.
#'
#' * **`elevatr`**
#'
#' Build base ggplot layer from digital elevation model data from AWS Open Data
#' Terrain Tiles via the [elevatr](https://github.com/jhollist/elevatr) package.
#' Map terrain tiles pulled from:
#'
#' - <https://registry.opendata.aws/terrain-tiles/>
#'
#' @param records Specimen voucher records [tibble::tibble()].
#' @param identifier Name of [`Specimen$records`][Specimen] tibble variable for
#'  filtering, annotations, and [ggplot2::ggplot()] aesthetics.
#' @param sf_border Character scalar to set color of
#'  county and state borders.
#' @param legend Character scalar to set legend title
#'  for `color` and `shape` keys via [ggplot2::labs()].
#' @param baselayer Specify map baselayer underlying county borders.
#'  One of: `base`, `ggmap`, or `elevatr`.
#' @param zoom Integer passed to [ggmap::get_map()] argument `zoom`.
#' @param center Numeric vector of length 2 specifying x,y lon/lat centroid.
#'  Default `NULL` centers map by [`Specimen$records`][Specimen] coordinate
#'  range midpoints.
#' @param maptype Choice of [ggmap::get_map()] `maptype` argument.
#'
#' @include specimens.R
#' @export
#'
#' @references
#' Hollister J, Shah T, Robitaille A, Beck M, Johnson M. 2021. `elevatr`:
#' Access Elevation Data from Various APIs. doi: 10.5281/zenodo.5809645.
#' R package version 0.4.2.
#' <https://github.com/jhollist/elevatr/>
#'
#' Kahle D and Wickham H. 2013. `ggmap`: Spatial Visualization with `ggplot2`.
#' The R Journal. 5(1):144-161.
#' <https://doi.org/10.32614/RJ-2013-014>
#'
#' Pebesma E. 2018. Simple Features for R: Standardized Support for Spatial
#' Vector Data. The R Journal. 10(1):438-446.
#' <https://doi.org/10.32614/RJ-2018-009>
#'
#' Teucher A, Russell K. 2021. `rmapshaper`: Client for 'mapshaper' for
#' 'Geospatial' operations. R package version 0.4.5.
#' <https://github.com/ateucher/rmapshaper>
#'
#' Walker K. 2016. tigris: An R package to access and work with
#' geographic data from the US census bureau.
#' The R Journal. 8(2):231-242.
#' <https://doi.org/10.32614/RJ-2016-043>
#'
#' @examples
#' voucher_map <- SpecimenMap$new(
#'   records = Thesis::herbarium_specimens,
#'   identifier = "Taxon_a_posteriori"
#' )
#'
#' voucher_map$limit(
#'   west = -108, east = -105,
#'   north = 42, south = 39
#' )
#'
#' # Base map type
#' voucher_map$map(
#'   legend = "Reviewed Annotation",
#'   sf_border = "black"
#' )
#'
#' # Example `elevatr` wrapper
#' voucher_map$map(
#'   legend = "Reviewed Annotation",
#'   sf_border = "grey",
#'   baselayer = "elevatr"
#' )
SpecimenMap <- R6::R6Class(
  classname = "SpecimenMap",
  inherit = Specimen,
  public = list(

    #' @description
    #' Construct record container [R6::R6Class()]
    #' subclass instance for geographic mapping.
    #'
    initialize = function(records, identifier) {
      super$initialize(records, identifier)
    },

    #' @description
    #' Layer census border shapefiles built from
    #' [tigris::states()] and [tigris::counties()] simple features.
    #' Data layers for simple feature (sf) objects are enabled by
    #' [ggplot2::geom_sf()]. Coordinate limits are set by the range of
    #' lon/lat values in the [`Specimen$records`][Specimen] tibble.
    #'
    #' @return List of state / county [ggplot2::geom_sf()] and
    #'  [ggplot2::coord_sf()] ggproto objects.
    features = function(sf_border) {
      list(
        ggplot2::geom_sf(
          data = private$counties(),
          inherit.aes = FALSE, size = 0.5, alpha = 0.75,
          color = sf_border, fill = NA
        ),
        ggplot2::geom_sf(
          data = private$states(),
          inherit.aes = FALSE, size = 1.2,
          color = sf_border, fill = NA
        ),
        ggplot2::coord_sf(
          xlim = base::range(self$records[["Longitude"]], na.rm = TRUE),
          ylim = base::range(self$records[["Latitude"]], na.rm = TRUE),
          expand = FALSE
        )
      )
    },

    #' @description
    #' Layer jitter geom of specimens from records tibble.
    #' Color and shape aesthetics are set by the
    #' [`Specimen$identifier`][Specimen] public field.
    #'
    #' @return List with [ggplot2::geom_jitter()] ggproto object.
    specimens = function() {
      # Order specimens to account for plot density
      specimens <- self$records %>%
        dplyr::add_count(.data[[self$identifier]]) %>%
        dplyr::arrange(dplyr::desc(.data$n)) %>%
        dplyr::filter(!is.na(.data$Longitude) & !is.na(.data$Latitude))
      list(
        ggplot2::geom_jitter(
          data = specimens,
          mapping = ggplot2::aes_string(
            x = "Longitude",
            y = "Latitude",
            color = self$identifier,
            shape = self$identifier
          ),
          size = 3
        )
      )
    },

    #' @description
    #' Layer manual scale values for color and shape aesthetics.
    #' Legend limits are subset to name values in
    #' [`Specimen$annotations()`][Specimen] for
    #' the set of species values indicated by [`Specimen$identifier`][Specimen].
    #' See [Thesis::aesthetics] for scale value specifications.
    #'
    #' @return List with [ggplot2::scale_color_manual()] and
    #'  [ggplot2::scale_shape_manual()] ggproto objects.
    scales = function() {
      manual_scales <- Thesis::aesthetics %>%
        dplyr::filter(species %in% names(self$annotations()))
      list(
        ggplot2::scale_color_manual(
          labels = self$annotations(),
          limits = manual_scales[["species"]],
          values = manual_scales[["color"]],
          na.value = "black"
        ),
        ggplot2::scale_shape_manual(
          labels = self$annotations(),
          limits = manual_scales[["species"]],
          values = manual_scales[["shape"]],
          na.value = 17
        )
      )
    },

    #' @description
    #' Layer [ggplot2::theme()] specification and default scale labels.
    #' Depends on [ggtext::element_markdown()] to set legend text for
    #' italicized species annotations using HTML formatting.
    #'
    #' @return List of [ggplot2::theme()] and [ggplot2::labs()] objects.
    theme = function(legend) {
      list(
        ggplot2::theme(
          panel.background = ggplot2::element_blank(),
          panel.border = ggplot2::element_rect(fill = NA, color = "black"),
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
          x = "Longitude", y = "Latitude",
          color = legend, shape = legend
        )
      )
    },

    #' @description
    #' Build [ggplot2::ggplot()] distribution map from the
    #' [`Specimen$records`][Specimen] field tibble. Color and shape aesthetics
    #' are set by the [`Specimen$identifier`][Specimen] field.
    #' Combines the public methods exposed by [Thesis::SpecimenMap].
    #'
    #' @return Grid graphics / ggplot object to print specimen distribution.
    map = function(legend = self$identifier,
                   sf_border = "black",
                   baselayer = c("base", "ggmap", "elevatr"),
                   zoom = 7,
                   center = NULL,
                   maptype = "satellite") {
      baselayer <- match.arg(baselayer, choices = c("base", "ggmap", "elevatr"))
      baselayer <-
        switch(baselayer,
          base = ggplot2::ggplot(),
          ggmap = private$base_ggmap(
            zoom = zoom,
            center = center,
            maptype = maptype
          ),
          elevatr = private$base_elevatr(zoom = zoom)
        )

      species_map <- baselayer +
        self$features(sf_border = sf_border) +
        self$specimens() +
        self$scales() +
        self$theme(legend = legend)
      return(species_map)
    },

    #' @description
    #' Layer botanist collection tags, wrapping over
    #' [ggrepel::geom_label_repel()] to plot repelled labels.
    #' Design pattern to forward multiple plot arguments modelled after:
    #'
    #' * <https://ggplot2-book.org/programming.html#additional-arguments>
    #'
    #' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Collector / collection
    #'  sets passed to [`Specimen$collections()`][Specimen].
    #' @param repel.params Arg/value pairs passed on to
    #'  [ggrepel::geom_label_repel()] to modify repelled labels.
    #'
    #' @examples
    #' voucher_map$map(
    #'   baselayer = "ggmap",
    #'   sf_border = "white"
    #' ) +
    #' voucher_map$repel(
    #'   Nelson = c(49286, 49478),
    #'   Kastning = c(1462, 1725),
    #'   repel.params = list(
    #'     xlim = c(-Inf, NA),
    #'     ylim = c(-Inf, NA),
    #'     segment.color = "#FFFFBF",
    #'     segment.size = 0.75,
    #'     segment.alpha = 1,
    #'     arrow = grid::arrow(length = grid::unit(0.01, "npc")),
    #'     box.padding = 0.5
    #'   )
    #' )
    repel = function(..., repel.params = list()) {
      records <- botanist(records = self$collections(...)$records)
      id <- do.call(what = ggrepel::geom_label_repel, utils::modifyList(
        list(
          data = records,
          mapping = ggplot2::aes(x = Longitude, y = Latitude, label = label),
          alpha = 0.5,
          max.overlaps = Inf,
          point.size = 3,
          box.padding = 0.5,
          min.segment.length = 0,
          segment.color = "black",
          segment.curvature = 0.25,
          segment.shape = 0.75
        ),
        val = repel.params
      ))
      return(id)
    }
  ),
  private = list(

    # Base Layer `ggmap` -------------------------------------------------------
    base_ggmap = function(zoom, center, maptype) {
      if (ggmap::has_google_key() == FALSE) {
        stop(paste(
          "Register an API key with Google.",
          "See: https://github.com/dkahle/ggmap"
        ))
      }
      maptype <-
        match.arg(
          arg = maptype,
          choices = as.character(formals(ggmap::get_map)[["maptype"]])
        )
      if (is.null(center)) {
        bound <- ggmap::make_bbox(
          lon = self$records[["Longitude"]],
          lat = self$records[["Latitude"]]
        )
        center <- c(
          lon = (bound[["left"]] + bound[["right"]]) / 2,
          lat = (bound[["bottom"]] + bound[["top"]]) / 2
        )
      } else {
        rlang::is_bare_numeric(center, n = 2) || rlang::abort(
          message = c(
            "Map centroid requires numeric vector of length 2.",
            "*" = glue::glue("`center` has a length of {length(center)}."),
            "i" = glue::glue("Set values for for lon (x) / lat (y) coordinates.")
          )
        )
      }
      ggmap_raster <-
        ggmap::get_map(location = center, zoom = zoom, maptype = maptype)
      ggmap_ggplot <- ggmap::ggmap(ggmap = ggmap_raster)
      return(ggmap_ggplot)
    },

    # Base Layer `elevatr` -----------------------------------------------------
    base_elevatr = function(zoom) {
      # Define projection and get AWS Open Data terrain tiles.
      prj_dd <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
      elev_raster <-
        elevatr::get_elev_raster(
          locations = as.data.frame(self$records[, c("Longitude", "Latitude")]),
          z = zoom, prj = prj_dd, clip = "bbox", src = "aws", verbose = FALSE
        )
      elev_df <-
        as.data.frame(
          methods::as(object = elev_raster, Class = "SpatialPixelsDataFrame")
        )
      elev_ggplot <-
        ggplot2::ggplot(
          data = elev_df,
          mapping = ggplot2::aes(x = .data$x, y = .data$y)
        ) +
        ggplot2::geom_tile(mapping = ggplot2::aes(fill = elev_df[, 1])) +
        ggplot2::scale_fill_gradientn("Elevation (m)",
          colours = grDevices::terrain.colors(7),
          guide = ggplot2::guide_colourbar(order = 1)
        )
      return(elev_ggplot)
    },

    # `tigris` State Borders ---------------------------------------------------
    states = function() {
      tigris_states <-
        mustashe::stash(
          var = "tigris_states",
          code = {
            tigris::states() %>%
              rmapshaper::ms_simplify(input = .)
          },
          functional = TRUE,
          verbose = FALSE
        )
      return(tigris_states)
    },

    # `tigris` County Borders --------------------------------------------------
    counties = function() {
      tigris_counties <- unique(self$records[["State"]]) %>%
        purrr::keep(
          .x = .,
          .p = ~ !(is.na(.x) | .x %in% c("Canada", "Unknown"))
        ) %>%
        purrr::map(
          .x = .,
          .f = function(state) {
            state_var <- stringr::str_to_lower(state) %>%
              stringr::str_replace(pattern = " +", replacement = "_")
            mustashe::stash(
              var = state_var,
              code = {
                tigris::counties(state = state, progress_bar = FALSE)
              },
              functional = TRUE,
              verbose = FALSE
            )
          }
        ) %>%
        tigris::rbind_tigris() %>%
        rmapshaper::ms_simplify(input = .)
      return(tigris_counties)
    }
  )
)

# Extract collector / collection label from records
botanist <- function(records) {
  collections <- records %>%
    dplyr::select(
      dplyr::all_of(
        c("Collector", "Collection_Number", "Longitude", "Latitude")
      )
    ) %>%
    dplyr::mutate(
      label = purrr::pmap_chr(
        .l = list(.data$Collector, .data$Collection_Number),
        .f = function(collector, collection) {
          extract <- stringr::str_extract_all(
            string = collector,
            pattern = "[A-Z][A-z']+(, Jr\\.)?"
          ) %>%
            unlist()
          botanist <- dplyr::case_when(
            length(extract) == 1 ~ list(as.character(extract)),
            length(extract) == 2 ~ list(paste(extract, collapse = " & ")),
            length(extract) >= 3 ~
              list(
                paste0(
                  paste(extract[1:(length(extract) - 1)], collapse = ", "),
                  ", & ", extract[length(extract)]
                )
              ),
            TRUE ~ list(NA_character_)
          )
          label <- paste(botanist[[1]], collection, sep = "\n")
          return(label)
        }
      )
    )
  return(collections)
}
