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

# ggplot Legends ----


#' Generate vector for `ggtext::element_markdown()` function call.
#'
#' Requires installation of `ggtext` as follows:
#'   remotes::install_github("clauswilke/ggtext")
#'
#' @param specimen_tibble Tibble subset of specimens to construct ggtext labels.
#' @param id_column Character vector matching specimen tibble ID column.
#' @return Character vector of html markup named by specimen identification.
#'
#' @export
#'
spp_labels <- function(specimen_tibble, id_column) {

  label_markdown <- function(label_vector) {
    purrr::map_chr(.x = label_vector, .f = function(label) {
      split_label <- unlist(strsplit(label, " "))
      if (length(split_label) %in% c(1, 2)) {
        # Genus with or without specific epithet.
        parsed_label <- paste0("*", label, "*")
      } else {
        if (grepl("ssp\\.", x = label)) {
          # Add html formatting to split ssp. onto second line.
          parsed_label <-
            paste0("*", paste0(split_label[1:2], collapse = " "),
                   "*", collapse = "") %>%
            paste0(., gsub(pattern = "s?sp\\.|var\\.", x = split_label[3],
              replacement = "<br><span>&nbsp;&nbsp;&nbsp;</span>  ssp\\. *"),
              split_label[4:length(split_label)], "*")
          } else {  # Any other cases
            parsed_label <- paste0("*", label, "*")
          }
        }
      return(parsed_label)
    })
  }

  # Add italics, html line break and non-breaking space characters.
  id_quo <- rlang::enquo(id_column)  # quosures for tidy evaluation.
  labels_vector <- specimen_tibble %>%
    dplyr::select(., !!id_quo) %>% dplyr::distinct(.) %>% dplyr::pull()
  labels_html <- label_markdown(labels_vector)
  names(labels_html) <- labels_vector

  # Return named vector of final identifications and markdown expression.
  return(labels_html)
}

