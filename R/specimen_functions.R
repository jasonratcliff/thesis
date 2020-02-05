require(tidyverse)
require(rlang)
require(plyr)

#' Split Numeric Ranges
#'
#' Given a tibble of range data in a character vector, split ranges separated
#' by "-" character.  Creates two new tibble columns containing numeric vectors
#' with minimum and maximum sorted numeric values split from character range.
#'
#' @param split_var Single column tibble of range data in character vector.
#' @return Tibble (3 columns) with character vector of raw ranges and
#'   two numeric vectors of min / max values from ranges split by "-".
#'
range_split <- function(split_var) {

  # Check input parameter is a single column tibble.
  if (!tibble::is_tibble(split_var) | ncol(split_var) != 1) {
    stop("`split_var` must be a sinlge column tibble trait range data")
  } else {
    # Count "-" instances and warn when multiple matches are detected.
    split_var %>% dplyr::pull() %>%
      purrr::map(function(range_element) {
        stringr::str_count(string = range_element, pattern = "-")
      }) %>% purrr::walk(function(element) {
        ifelse(length(element) > 1, warning("Multiple '-' detected."), element)
      })
  }

  # Split character vector of "-" separated range into list of sorted numerics.
  split_var %>% dplyr::pull() %>%
    purrr::map(function(range_element) {
      if (grepl(pattern = "-", x = range_element)) {
        str_split(string = range_element, pattern = "-") %>% unlist() %>%
          as.numeric() %>% sort()
      } else { as.numeric(c(range_element, range_element)) }
    }) %>%

    # Cast list of split range data into tibble and add column of original data.
    plyr::ldply(.data = ., .fun = rbind) %>% tibble::as_tibble() %>%
    dplyr::mutate(split_var = split_var %>% dplyr::pull()) %>%
    dplyr::select(split_var, `1`, `2`) %>%

    # Rename tibble variables wth minimum and maximum range data.
    # https://dplyr.tidyverse.org/articles/programming.html#different-input-and-output-variable
    dplyr::rename(!!names(split_var) := split_var,
                  !!paste0(names(split_var), "_min") := `1`,
                  !!paste0(names(split_var), "_max") := `2`)

}

#' Subset specimen data by geographic coordinates.
#' 
#' Given an input data.frame and at least one of latitude or longitude
#' vectors with minimum and maximum values to subset coordinate data.
#' 
#' @param specimen_df Data.frame with columns Latitude / Longitude.
#'   Should be numeric vectors of decimal degree coordinates.
#' @param Latitude Character vector length two min / max latitude.
#' @param Longitude Character vector length two min / max longitude.
#' @return Data.frame of specimens within coordinate bounds.
#' @examples
#' \dontrun{
#' specimen_subset <- subset_coords(specimen_df = total_physaria,
#'                      Latitude = c(41, 45), Longitude = c(-109, -105))
#' range(specimen_subset$Latitude)
#' range(specimen_subset$Longitude)
#' }
subset_coords <- function(specimen_df, Latitude = NULL, Longitude = NULL) {
  
  # Test for at list one of latitude / longitude vectors.
  if (is.null(Latitude) & is.null(Longitude)) {
    stop("Enter a vector for subsetting coordinates.")
  }
  
  # Assign list of coordinates and test that values are ranges.
  filter_coordinates <-
    list("Longitude" = Longitude, "Latitude" = Latitude) %>%
    lapply(X = ., function(coordinate) {
      if (!is.null(coordinate) & length(coordinate) != 2) {
        stop("Enter a coordinate range of two values.")
      } else {
        return(coordinate)
      }
    })
  
  # Map coordinate tibble ranges with minimum / maximum coordinate values.
  coordinate_tbl <-
    filter_coordinates[which(lapply(filter_coordinates, is.null) == FALSE)] %>%
    purrr::map2_dfr(.x = ., .y = names(.),
      .f =  function(coordinate, parallel_name) {
        if (identical(!unique(coordinate > 0), TRUE) ||
            identical(!unique(coordinate < 0), TRUE)) {
          coordinate_ranges <- range(coordinate)
          } else {
            warning("Coordinates not in the same hemisphere...")
            }
        dplyr::bind_rows(
          dplyr::bind_cols(parallel = parallel_name, edge = "minimum", 
                           coordinate = min(coordinate_ranges)),
          dplyr::bind_cols(parallel = parallel_name, edge = "maximum",
                           coordinate = max(coordinate_ranges)))
        })
  
  # Construct dplyr filter statements from mapped coordinate tibble.
  filter_statements <-
    purrr::pmap_dfr(coordinate_tbl, function(parallel, edge, coordinate) {
      operand <- ifelse(edge == "minimum", ">", "<")
      filter_statement <- paste(parallel, operand, coordinate, collapse = " ")
      bind_cols(construct = filter_statement)
    }) %>% purrr::pluck("construct") %>% paste0("filter(", .) %>% paste0(., ")")
  
  # Construct data filter pipeline by coordinate ranges and return evaluation.
  filter_expr <- paste("specimen_df %>%",
                       paste(filter_statements[1:length(filter_statements) - 1],
                             collapse = " %>% "), "%>%",
                       filter_statements[length(filter_statements)])
  eval(rlang::parse_expr(filter_expr))
}

