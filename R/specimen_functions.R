# Specimen Access Tools ----

#' Subset specimen data by geographic coordinates.
#'
#' Given an input data.frame and at least one of latitude or longitude
#' vectors with minimum and maximum values to subset coordinate data.
#'
#' @param specimen_tbl Data.frame with columns Latitude / Longitude.
#'   Should be numeric vectors of decimal degree coordinates.
#' @param Latitude Character vector length two min / max latitude.
#' @param Longitude Character vector length two min / max longitude.
#' @importFrom rlang .data
#'
#' @return Data.frame of specimens within coordinate bounds.
#'
#' @examples
#' specimen_subset <- subset_coords(specimen_tbl = herbarium_specimens,
#'                                  Latitude = c(41, 45),
#'                                  Longitude = c(-109, -105))
#' range(specimen_subset$Latitude)
#' range(specimen_subset$Longitude)
#'
#' @export
#'
subset_coords <- function(specimen_tbl, Latitude = NULL, Longitude = NULL) {

  # Cast non-null inputs as numerically sorted character vectors.
  args_cords <- list(Longitude = Longitude, Latitude = Latitude) %>%
    purrr::map(function(coords) sort(coords)) %>%
    purrr::keep(~ !rlang::is_null(.) & length(.) > 0) %>%
    purrr::modify_if(.p = ~ length(.) == 2, .f = as.double)
  purrr::walk(args_cords, ~ if(length(.) != 2) {
    warning("Missing Coordinates: ", .)  # Verify non-missing coordaintes
  })

  # Map inputs and argument names to create function call objects.
  args_sym <- names(args_cords)
  coords_expr <-
    purrr::map2(args_sym, args_cords, function(parallel, coords) {
      coord_sym <- rlang::sym(parallel)
      min_filter <- rlang::call2(rlang::expr(dplyr::filter),
                                 rlang::expr(!!coord_sym > !!coords[1]))
      max_filter <- rlang::call2(rlang::expr(dplyr::filter),
                                 rlang::expr(!!coord_sym < !!coords[2]))
      c(min_filter, max_filter)
    }) %>% unlist() %>%
    purrr::reduce(~ rlang::expr(!!.x %>% !!.y),
                  .init = rlang::expr(specimen_tbl))

  # Return evaluated call object.
  rlang::eval_tidy(rlang::expr(!!coords_expr))
}

#' Find specimens by Collector and Collection Number.
#'
#' @param specimen_tbl Tibble of herbarium specimens built from
#'  `herbarium_specimens.R` script.
#' @param collector Character scalar to filter specimen by collector name.
#' @param collection Numeric vector to filter specimens by collection number.
#'
#' @examples
#' find_spp(herbarium_specimens, collector = "Rollins")
#' find_spp(herbarium_specimens, collector = "Rollins", collection = 81337)
#'
#' @export
#'
find_spp <- function(specimen_tbl, collector = NULL, collection = NULL) {

  # Keep non-null arguments from list of function inputs.
  args_find <- list(Collector = collector,
                    Collection_Number = as.character(collection)) %>%
    purrr::keep(~ !rlang::is_null(.x) & length(.x) > 0)

  # Map inputs and argument names to create function call objects.
  args_sym <- names(args_find)
  spp_filter <-
    purrr::map2(args_sym, args_find,
      ~ rlang::call2(rlang::expr(dplyr::filter),
          rlang::call2(rlang::expr(stringr::str_detect),
                       string = rlang::sym(.x), pattern = .y))) %>%
    purrr::reduce(~ rlang::expr(!!.x %>% !!.y),
                  .init = rlang::expr(specimen_tbl))

  # Return evaluated call object.
  rlang::eval_tidy(spp_filter)
}

