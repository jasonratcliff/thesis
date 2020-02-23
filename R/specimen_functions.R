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
      dplyr::bind_cols(construct = filter_statement)
    }) %>% purrr::pluck("construct") %>%
    paste0("dplyr::filter(", .) %>% paste0(., ")")

  # Construct data filter pipeline by coordinate ranges and return evaluation.
  filter_expr <- paste("specimen_tbl %>%",
                       paste(filter_statements[1:length(filter_statements) - 1],
                             collapse = " %>% "), "%>%",
                       filter_statements[length(filter_statements)])
  eval(rlang::parse_expr(filter_expr))
}

#' Find specimens by Collector, Collection Number, or Row ID
#'
#' @param specimen_tbl Tibble of herbarium specimens built from
#'  `herbarium_specimens.R` script.
#' @param collector Character scalar to filter specimen by collector name.
#' @param collection Numeric vector to filter specimens by collection number.
#' @param row_id Numeric vector of row indexes to slice tibble.
#'
#' @examples
#' find_spp(herbarium_specimens, collector = "Rollins")
#' find_spp(herbarium_specimens, collector = "Rollins", collection = 81337)
#' find_spp(herbarium_specimens, row_id = c(1:12))
#'
#' @export
#'
find_spp <- function(specimen_tbl, collector = NULL, collection = NULL,
                     row_id = numeric()) {

  # Check for input tibble.
  if (!tibble::is_tibble(specimen_tbl)) {
    stop("Pass a tibble object for `specimen_tbl` argument.")
  }

  # Test arguments to warn when `row_id` and `collector` or `collection`.
  test_args <- rlang::enquos(collector, collection)
  names(test_args) <- c("collector", "collection")
  if (length(row_id) > 0 &
      TRUE %in% c(!is.null(collector) | !is.null(collection))) {
    null_args <- purrr::map_lgl(test_args, function(arg) {
      is.null(rlang::eval_tidy(arg))
    })
    warning(paste0("Arguments `row_id` and `",
                   paste(names(null_args)[which(null_args == FALSE)],
                         collapse = "` & `"), '` have been passed.'))
  }

  # Select columns from taxa data frame to return.
  filtered_spp <- specimen_tbl %>%
    dplyr::select("Taxon", "prior_id", "Taxon_a_posteriori",
                  "Collector", "Collection_Number",
                  "Latitude", "Longitude", "State", "County", "Herbarium")

  # Slice by row index.
  if (is.numeric(row_id) & length(row_id) > 0) {
    filtered_spp <- dplyr::slice(filtered_spp, row_id)
  } else if (!is.numeric(row_id)) {
    warning("Ensure `row_id` is a numeric vector.")
  }

  # Map collector / collection enquosures and parse filter expression.
  names(test_args) <- c("Collector", "Collection_Number")
  filter_statements <-  # Keep non-null enquosures.
    purrr::keep(test_args, function(arg) {
    !is.null(rlang::eval_tidy(arg))
  }) %>% unlist() %>%
    # Build filter statements with quoted Collector as character vector.
    purrr::map2(., names(.), function(arg, variable) {
      paste0("grepl(x = ", variable, ", pattern = ",
             ifelse(variable == "Collector",
                    paste0("'", rlang::eval_tidy(arg), "'"),
                    rlang::eval_tidy(arg)), ")")  # Unquoted collection number.
    }) %>% unlist() %>% paste(collapse = ", ") %>%
    paste0("dplyr::filter(", ., ")", collapse = "")

  # Construct data filter pipeline to find specimens by collector / collection.
  filter_expr <- paste("filtered_spp %>%", filter_statements, collapse = "")
  eval(rlang::parse_expr(filter_expr))
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

