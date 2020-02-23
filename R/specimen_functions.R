# General Trait Parsing ----

#' Split Numeric Ranges
#'
#' Given a tibble of range data in a character vector, split ranges separated
#' by "-" character.  Creates two new tibble columns containing numeric vectors
#' with minimum and maximum sorted numeric values split from character range.
#'
#' @param split_var Single column tibble of range data in character vector.
#' @importFrom rlang .data :=
#'
#' @return Tibble (3 columns) with character vector of raw ranges and
#'   two numeric vectors of min / max values from ranges split by "-".
#'
#' @examples
#' herbarium_specimens %>%
#'   dplyr::select(Ovule_number) %>% dplyr::filter(!is.na(Ovule_number)) %>%
#'    range_split(split_var = .)
#' @export
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
        stringr::str_split(string = range_element, pattern = "-") %>%
          unlist() %>% as.numeric() %>% sort()
      } else { as.numeric(c(range_element, range_element)) }
    }) %>%

    # Cast list of split range data into tibble and add column of original data.
    plyr::ldply(fun = rbind) %>% tibble::as_tibble() %>%
    dplyr::mutate(split_var = split_var %>% dplyr::pull()) %>%
    dplyr::select(split_var, .data$V1, .data$V2) %>%

    # Rename tibble variables wth minimum and maximum range data.
    # https://dplyr.tidyverse.org/articles/programming.html#different-input-and-output-variable
    dplyr::rename(!!names(split_var) := split_var,
                  !!paste0(names(split_var), "_min") := "V1",
                  !!paste0(names(split_var), "_max") := "V2")

}

