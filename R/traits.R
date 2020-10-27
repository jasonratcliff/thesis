# Range Data ----

#' Split Numeric Ranges
#'
#' Given a tibble of range data in a character vector, split ranges separated
#' by "-" character.  Creates two new tibble columns containing numeric vectors
#' with minimum and maximum sorted numeric values split from character range.
#'
#' @param trait_tbl Tibble of specimen observations to split range data from.
#' @param split_var Single column tibble of range data in character vector.
#' @importFrom rlang .data :=
#' @export
#'
#' @return Tibble (3 columns) with character vector of raw ranges and
#'   two numeric vectors of min / max values from ranges split by "-".
#'
#' @examples
#' herbarium_specimens %>%
#'   dplyr::select(Ovule_number) %>% dplyr::filter(!is.na(Ovule_number)) %>%
#'    range_split(trait_tbl = ., split_var = "Ovule_number")
#' @export
#'
range_split <- function(trait_tbl, split_var) {

  # Count "-" instances and warn when multiple matches are detected.
  dplyr::pull(trait_tbl, split_var) %>%
    purrr::map(function(element) {
      stringr::str_count(string = element, pattern = "-")
    }) %>%
    purrr::walk2(.x = ., .y = seq_along(.), function(element, index) {
      ifelse(element > 1,
             warning("Multiple '-' detected at index: ", index),  element)
    })

  # Split character vector of "-" separated range into list of sorted numerics.
  dplyr::pull(trait_tbl, split_var) %>%
    purrr::map(function(element) {
      if (grepl(pattern = "-", x = element)) {
        stringr::str_split(string = element, pattern = "-") %>%
          unlist() %>% as.numeric() %>% sort()
      } else { as.numeric(c(element, element)) }
    }) %>%

    # Cast list of split range data into tibble and add column of original data.
    plyr::ldply(fun = rbind) %>%
    tibble::as_tibble(.name_repair = "minimal") %>%
    dplyr::mutate(!!split_var := dplyr::pull(trait_tbl, split_var)) %>%
    dplyr::select(1:2) %>%

    # Rename tibble variables wth minimum and maximum range data.
    # https://dplyr.tidyverse.org/articles/programming.html#different-input-and-output-variable
    dplyr::rename(!!paste0(split_var, "_min") := "V1",
                  !!paste0(split_var, "_max") := "V2")

}

