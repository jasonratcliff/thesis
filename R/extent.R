# TODO Handle assignment of retained specimens to `private$.filtered`

#' @title Specimen spatial extent
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Given a set of specimen records with valid geographic coordinates,
#' define simple features via [sf](https://r-spatial.github.io/sf/index.html).
#'
#' * [Geometry types](https://r-spatial.github.io/sf/articles/sf1.html#simple-feature-geometry-types)
#'
#' @param records Specimen vouchers [tibble::tbl_df] data frame.
#' @export
Extent <- R6::R6Class(
  classname = "Extent",
  parent_env = rlang::env_parent(),
  private = list(
    .bounds = c("xmin", "ymin", "xmax", "ymax"),
    .records = NULL,
    .filtered = NULL,
    .crs = NULL,
    .bbox = NULL
  ),
  active = list(
    #' @field records Access [`tbl_df`][tibble::tbl_df-class] class data frame
    #'   of retained specimen records.
    records = function() {
      filtered <- private$.filtered %||% private$.records
      return(filtered)
    },
    #' @field crs Get / set coordinate reference system (CRS) for projection.
    crs = function(value) {
      if (missing(value)) {
        return(private$.crs)
      } else {
        private$.crs <- sf::st_crs(value)
        rlang::inform(message = c("Setting CRS:", private$.crs$input))
      }
    },
    #' @field sf Construct simple features geometry from specimen coordinates.
    sf = function() {
      filtered <- dplyr::filter(self$records, validCoordinates) |>
        # TODO Understand spherical geometry operations with geodetic CRS.
        sf::st_as_sf(coords = c("decimalLongitude", "decimalLatitude"))
      if (!is.null(private$.bbox)) {
        bounding <- sf::st_as_sfc(sf::st_bbox(private$.bbox))
        intersection <- sf::st_intersects(bounding, filtered)
        filtered <- filtered[intersection[[1]], ]
      }
      return(filtered)
    }
  ),
  public = list(
    #' @description Instantiate specimen record extent for simple features.
    #'
    #' @examples
    #' (extent <- Extent$new(records = vouchers))
    initialize = function(records) {
      if (!inherits(records, "tbl_df")) {
        rlang::abort(c("Input `records` must inherit `tbl_df`:", class(records)))
      }

      private$.records <- thesis:::unique_ids(records) |>
        dplyr::select(dplyr::any_of(thesis:::dwc), dplyr::everything()) |>
        dplyr::mutate(
          validCoordinates = !is.na(decimalLongitude) & !is.na(decimalLatitude)
        )

      # Default coordinate reference system and bounding from coordinates.
      private$.crs <- sf::st_crs("EPSG:4326")
      private$.bbox <- sf::st_bbox(self$sf)
    },
    #' @description Define bounding to intersect with projected specimens.
    #'
    #' See `sf::st_bbox()` for description of the `bbox` class
    #'
    #' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Named numeric scalars to
    #' set boundary box. Any of `xmin`, `ymin`, `xmax`, `ymax`.
    #' @examples
    #' # Initial boundary box set from specimens with non-missing coordinates.
    #' extent$bbox()
    #'
    #' # Utilize reference semantics to update bounding for specimen subset.
    #' extent$bbox(xmin = -109, ymin = 39, xmax = -105, ymax = 42)
    #' extent$bbox()
    bbox = function(...) {
      if (missing(...)) {
        return(private$.bbox)
      } else {
        matched <- thesis:::match_bbox(..., .bounds = private$.bounds)
        private$.bbox[names(matched)] <- matched
      }
    }
  )
)

# Ensure unique & non-missing collectionID variable in $records field.
unique_ids <- function(records) {
  if (!"collectionID" %in% names(records)) {
    rlang::inform(c("Creating unique record identifier:", "collectionID"))
    records <- records |>
      dplyr::mutate(collectionID = seq_len(nrow(records))) |>
      dplyr::relocate(dplyr::matches("collectionID"), .before = 1)
  } else {
    ids <- unique(records$collectionID)
    if (length(ids[!is.na(ids)]) != nrow(records)) {
      rlang::abort(c("Unique values required for records in:", "collectionID"))
    }
  }
  return(records)
}

# Match {1:4} bounds simple features boundings: [ xmin, ymin, xmax, ymax ]
match_bbox <- function(..., .bounds) {
  dots <- rlang::dots_list(..., .named = TRUE)
  bounds <- rlang::set_names(as.numeric(dots), names(dots))
  matched <- names(bounds) %in% .bounds
  if (length(bounds) > sum(matched)) {
    rlang::warn(
      c("Un-used arguments:", names(bounds)[which(!matched)])
    )
  }
  return(bounds[matched])
}

# Terms indexing ---------------------------------------------------------------

# Set of pre-defined Darwin Core (DwC) terms to describe records.
dwc <- {
  c(
    "collectionID", "datasetID", "institutionCode", "scientificName",
    "organismName", "previousIdentifications", "recordedBy", "recordNumber",
    "associatedTaxa", "occurrenceRemarks", "verbatimEventDate", "eventDate",
    "year", "month", "day", "habitat", "stateProvince", "county",
    "verbatimLocality", "decimalLatitude", "decimalLongitude",
    "minimumElevationInMeters", "maximumElevationInMeters", "verbatimElevation",
    "verbatimIdentification", "typeStatus"
  )
}
