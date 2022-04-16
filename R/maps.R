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
#' @param records Specimen voucher records [tibble::tibble()].
#' @param identifier Name of [`Specimen$records`][Specimen] tibble
#'  variable to designate annotation type for filtering, annotations,
#'  and [ggplot2::ggplot()] aesthetics.
#' @param sf_border Character scalar to set color of
#'  county and state borders.
#' @param legend Character scalar to set legend title
#'  for `color` and `shape` keys via [ggplot2::labs()].
#'
#' @include specimens.R
#' @export
#'
#' @references
#' Pebesma E. 2018. Simple feature for R: Standardized support for spatial
#'   vector data. The R Journal. 10(1):438-446.
#'
#' Teucher A, Russell K. 2021. rmapshaper: Client for 'mapshaper' for
#'   'Geospatial' operations. R package version 0.4.5.
#'
#' Walker K. 2016. tigris: An R package to access and work with
#'   geographic data from the US census bureau. The R Journal. 8(2):231-242.
#'
#' @examples
#' voucher_map <- SpecimenMap$new(
#'   records = Thesis::herbarium_specimens,
#'   identifier = "Taxon_a_posteriori"
#' )
#'
#' voucher_map$taxa(
#'   c("medicinae", "vitulifer", "floribunda"),
#'   identifier = "Taxon_a_posteriori"
#' )
#'
#' voucher_map$map()
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
    #' lon/lat values in the [records] tibble.
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
          ylim = base::range(self$records[["Latitude"]], na.rm = TRUE)
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
      list(
        ggplot2::geom_jitter(
          mapping = ggplot2::aes_string(
            x = "Longitude",
            y = "Latitude",
            color = self$identifier,
            shape = self$identifier
          ),
          size = 3, alpha = 0.75,
          width = 0.1, height = 0.1
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
    #' Build [ggplot2::ggplot()] distribution map from  `records` field
    #' specimens. Color and shape aesthetics are set by the `identifier` field.
    #' Combines the public methods exposed by [Thesis::SpecimenMap].
    #'
    #' @return Grid graphics / ggplot object to print specimen distribution.
    map = function(legend = self$identifier, sf_border = "black") {

      # Order specimens to account for plot density
      specimens <- self$records %>%
        dplyr::add_count(.data[[self$identifier]]) %>%
        dplyr::arrange(dplyr::desc(.data$n)) %>%
        dplyr::filter(!is.na(.data$Longitude) & !is.na(.data$Latitude))

      species_map <- ggplot2::ggplot(data = specimens) +
        self$features(sf_border) +
        self$specimens() +
        self$scales() +
        self$theme(legend = legend)
      return(species_map)
    }
  ),
  private = list(

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
