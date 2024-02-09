# Specimen Mapping ------------------------------------------------------------

#' @title Specimen Mapping
#'
#' @description
#' This R6 subclass inherits the [thesis::Specimen] superclass,
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
#' @param .legend Character scalar to set legend title
#'  for `color` and `shape` keys via [ggplot2::labs()].
#' @param zoom Integer passed to [ggmap::get_map()] argument `zoom`.
#' @param center Numeric vector of length 2 specifying x,y lon/lat centroid.
#'  Default `NULL` centers map by [`Specimen$records`][Specimen] coordinate
#'  range midpoints.
#' @param maptype Choice of [ggmap::get_map()] `maptype` argument.
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
#'   records = thesis::vouchers,
#'   identifier = "scientificName"
#' )
#'
#' voucher_map$filter_limit(
#'   west = -108, east = -105,
#'   north = 42, south = 39
#' )
#'
#' # Base map type
#' voucher_map$map(
#'   .legend = "Reviewed Annotation",
#'   .borders = "black"
#' )
#'
#' # Example `elevatr` wrapper
#' voucher_map$map(
#'   .legend = "Reviewed Annotation",
#'   .borders = "grey",
#'   baselayer = "elevatr"
#' )
#'
#' @include specimens.R
#' @export
SpecimenMap <- R6::R6Class(
  classname = "SpecimenMap",
  inherit = Specimen,
  private = list(
    .source = NULL,
    .baselayer = NULL,
    .states = NA,
    .counties = NA,
    .borders = "black",
    .expand = FALSE,
    ggmap = list(
      key = function() {
        if (!ggmap::has_google_key()) {
          rlang::abort(
            message = c(
              "Register API key with Google:",
              "https://github.com/dkahle/ggmap#google-maps-api-key"
            )
          )
        }
      },
      maptype = function(.maptype) {
        match.arg(arg = .maptype, choices = ggmap:::GOOGLE_VALID_MAP_TYPES)
      }
    )
  ),
  public = list(
    #' @description
    #' Construct record container [R6::R6Class()]
    #' subclass instance for geographic mapping.
    #'
    initialize = function(records, identifier = NULL) {
    #' @param records Specimen records tibble as defined for `Specimen$new()`.
    #' @param identifier Darwin Core term matching column in `$records` to set
    #'  default variable for specimen plot map aesthetics.
    #' @param baselayer Set map baselayer type by source package, one of:
    #'  * `ggplot2`: Basic plot with simple features from `tigris` shapefiles
    #'  * `ggmap`: Google Maps API via `ggmap::get_map()`
    #'  * `elevatr`: Elevation rasters via [`elevatr`](https://github.com/jhollist/elevatr)
    initialize = function(records, identifier = NULL,
                          baselayer = c("ggplot2", "ggmap", "elevatr"),
                          ggmap.params = list(
                            location = NULL,
                            maptype = "satellite",
                            zoom = 7)) {
      super$initialize(records, identifier)
      private$.source <-
        match.arg(baselayer, choices = c("ggplot2", "ggmap", "elevatr"))
      private$.baselayer <-
        switch(
          EXPR = private$.source,
          ggplot2 = {
            ggplot2::ggplot(
              data = self$records,
              mapping = ggplot2::aes(
                x = decimalLongitude,
                y = decimalLatitude
              )
            )
          },
          ggmap = {
            if (private$ggmap$key() %||% TRUE) {
              .location <- ggmap.params[['location']] %||%
                c(lon = sum(private$.bb_lon) / 2, lat = sum(private$.bb_lat) / 2)
              ggmap::ggmap(
                ggmap = ggmap::get_map(
                  location = .location,
                  maptype = private$ggmap$maptype(ggmap.params[['maptype']]),
                  zoom = ggmap.params[['zoom']]
                )
              )
            }
          },
          elevatr = stop("Unimplemented")
        )

      # Define private fields for `tigris` border shapefiles.
      private$.states <- tigris::states()
      private$.counties <-
        tigris::counties(
          state = purrr::keep(
            .x = unique(self$records$stateProvince),
            .p = \(x) x %in% datasets::state.name
          )
        )
    },
    #' @description Construct `ggplot2` from composed plot layers.
    #' @return Grid graphics / ggplot object to print specimen distribution.
    cartography = function() {
      private$.baselayer +
        self$states +
        self$counties +
        self$coordinates +
        self$specimens() +
        self$scales() +
        self$theme()
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
        dplyr::filter(
          !is.na(.data$decimalLongitude) &
            !is.na(.data$decimalLatitude)
        )

      list(
        ggplot2::geom_jitter(
          data = specimens,
          mapping = ggplot2::aes(
            x = decimalLongitude,
            y = decimalLatitude,
            color = .data[[self$identifier]],
            shape = .data[[self$identifier]]
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
    #' See [thesis::aesthetics] for scale value specifications.
    #'
    #' @return List with [ggplot2::scale_color_manual()] and
    #'  [ggplot2::scale_shape_manual()] ggproto objects.
    scales = function() {
      manual_scales <- thesis::aesthetics %>%
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
    theme = function(.legend = NULL) {
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
          x = "decimalLongitude", y = "decimalLatitude",
          color = .legend, shape = .legend, size = .legend
        )
      )
    },
    #' @description
    #' Layer botanist collection tags, wrapping over
    #' [ggrepel::geom_label_repel()] to plot repelled labels.
    #' Design pattern to forward multiple plot arguments modelled after:
    #'
    #' * <https://ggplot2-book.org/programming.html#additional-arguments>
    #'
    #' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Sets of `recordedBy` /
    #'   `recordNumber` pairs passed to [`Specimen$filter_collections()`][Specimen].
    #' @param vouchers Specimen records [`tbl_df`][tibble::tbl_df-class] to
    #'  specify subset of record collections. Requires columns
    #'  `recordedBy`, `recordNumber`, `decimalLongitude`, and `decimalLatitude`
    #'   to construct repelled labels.
    #' @param repel.params Arg/value pairs passed on to
    #'  [ggrepel::geom_label_repel()] to modify repelled labels.
    #'
    #' @examples
    #' voucher_map$map(
    #'   baselayer = "ggmap",
    #'   .borders = "white"
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
    repel = function(..., vouchers = NULL, repel.params = list()) {
      filtered <- self$filter_collections(..., .return = TRUE)
      collections <- rlang::list2(...)
      if (length(collections) > 0) {
        collectors <- botanist(records = filtered)
      }
      if (!is.null(vouchers)) {
        collectors <- tryCatch(
          dplyr::bind_rows(botanist(records = vouchers), collectors),
          error = function(e) botanist(records = vouchers)
        )
      }

      id <- do.call(what = ggrepel::geom_label_repel, utils::modifyList(
        list(
          data = collectors,
          mapping = ggplot2::aes(
            x = decimalLongitude,
            y = decimalLatitude,
            label = label
          ),
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
    },

    # Base Layer `elevatr` -----------------------------------------------------
    base_elevatr = function(zoom) {
      # Define projection and get AWS Open Data terrain tiles.
      prj_dd <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
      elev_raster <-
        elevatr::get_elev_raster(
          locations = as.data.frame(
            self$records[, c("decimalLongitude", "decimalLatitude")]
          ),
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
    }
  ),
  active = list(
    #' @field states Layer [ggplot2::geom_sf()] simple features from state
    #'   boundary shapefiles returned by [tigris::states()].
    states = function() {
      ggplot2::geom_sf(
        data = private$.states_sf,
        color = private$.borders, fill = NA,
        size = 1.2, alpha = 0.75,
        inherit.aes = FALSE
      )
    },
    #' @field counties Layer [ggplot2::geom_sf()] simple features from county
    #'   boundary shapefiles returned by [tigris::counties()].
    counties = function() {
      ggplot2::geom_sf(
        data = private$.counties_sf,
        color = private$.borders, fill = NA,
        size = 0.5, alpha = 0.75,
        inherit.aes = FALSE
      )
    },
    #' @field coordinates Layer [ggplot2::coord_sf()] to decimal degree
    #' format coordinates from extent of `self$records` specimen records.
    coordinates = function() {
      ggplot2::coord_sf(
        xlim = private$.bb_lon,
        ylim = private$.bb_lat,
        expand = private$.expand
      )
    }
  )
)

# Extract collector / collection label from records
botanist <- function(records) {
  collections <- records %>%
    dplyr::select(
      dplyr::all_of(
        c(
          "recordedBy", "recordNumber",
          "decimalLongitude", "decimalLatitude"
        )
      )
    ) %>%
    dplyr::mutate(
      label = purrr::pmap_chr(
        .l = list(.data$recordedBy, .data$recordNumber),
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
