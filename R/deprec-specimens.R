#' Count Specimens
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' Tabulate the total number of distinct specimen collections by collector,
#' colleciton number and date while accounting for herbarium duplicates.
#'
#' @param spp_tibble Specimen tibble to count unique specimen vouchers.
#' @export
#'
#' @return Numeric scalar with the number of distinct specimen vouchers.
#'
#' @examples
#' count_specimens(spp_tibble = herbarium_specimens)
#'
#' @keywords internal
count_specimens <- function(spp_tibble) {
  # Calculate total number of specimens accounting for duplicated records.
  specimen_count <- spp_tibble %>%
    dplyr::select("Collector", "Collection_Number", "Date", "Herbarium") %>%
    dplyr::mutate(
      row_id = 1:nrow(.),
      Herbarium = stringr::str_remove_all(
        string = .data$Herbarium,
        pattern = "\\[|\\]"
      ) %>%
        stringr::str_split(string = ., pattern = ", +") %>%
        purrr::map_chr(., function(herbarium) {
          herbarium_split <- unlist(herbarium) %>%
            sort() %>%
            stringr::str_c(., collapse = " ")
          ifelse(length(herbarium_split) == 1, herbarium_split, "NA")
        })
    ) %>%
    dplyr::distinct(., .data$Collector, .data$Collection_Number,
      .data$Date, .data$Herbarium,
      .keep_all = TRUE
    ) %>%
    nrow()
  return(specimen_count)
}

# Access Tools ----

#' Filter Reviewed Specimens
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' Subset specimen tibble to exclude outgroups, leaving only species of interest.
#'
#' @details
#' Reviewed species include:
#' - Physaria
#' - P. acutifolia
#' - P. brassicoides
#' - P. condensata
#' - P. didymocarpa subsp. didymocarpa
#' - P. didymocarpa subsp. lanata
#' - P. didymocarpa subsp. lyrata
#' - P. dornii
#' - P. eburniflora
#' - P. integrifolia
#' - P. 'medicinae'
#' - P. vitulifera
#'
#' @inheritParams layer_specimens
#' @export
#'
#' @return Tibble of specimens filtered by species of reviewed annotation.
#'
#' @examples
#' herbarium_specimens %>%
#'   filter_reviewed(specimen_tbl = .) %>%
#'   dplyr::select(Taxon_a_posteriori) %>%
#'   dplyr::add_count(Taxon_a_posteriori) %>%
#'   dplyr::distinct()
#'
#' @keywords internal
filter_reviewed <- function(specimen_tbl) {
  filtered_specimens <- specimen_tbl %>%
    filter(
      !grepl(
        pattern = "outgroup|excluded",
        x = .data$excel_sheet
      ),
      !grepl(
        pattern = paste("chambersii", "floribunda", "rollinsii", sep = "|"),
        x = .data$Taxon_a_posteriori
      )
    )
  return(filtered_specimens)
}

#' Find specimens by Collector and Collection Number.
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' @param specimen_tbl Tibble of herbarium specimens built from
#'  `vouchers.R` script.
#' @param collector Character scalar to filter specimen by collector name.
#' @param collection Numeric vector to filter specimens by collection number.
#'
#' @examples
#' find_spp(herbarium_specimens, collector = "Rollins")
#' find_spp(herbarium_specimens, collector = "Rollins", collection = 81337)
#'
#' @export
#'
#' @keywords internal
find_spp <- function(specimen_tbl, collector = NULL, collection = NULL) {
  # Keep non-null arguments from list of function inputs.
  args_find <- list(
    Collector = collector,
    Collection_Number = as.character(collection)
  ) %>%
    purrr::keep(~ !rlang::is_null(.x) & length(.x) > 0)

  # Map inputs and argument names to create function call objects.
  args_sym <- names(args_find)
  spp_filter <-
    purrr::map2(
      args_sym, args_find,
      ~ rlang::call2(
        rlang::expr(dplyr::filter),
        rlang::call2(rlang::expr(stringr::str_detect),
          string = rlang::sym(.x), pattern = .y
        )
      )
    ) %>%
    purrr::reduce(~ rlang::expr(!!.x %>% !!.y),
      .init = rlang::expr(specimen_tbl)
    )

  # Return evaluated call object.
  rlang::eval_tidy(spp_filter)
}
