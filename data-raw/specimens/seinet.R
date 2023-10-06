## =============================================================================
##
## Title: SEINet Occurrence R Data
##
## Author: jasonratcliff
##
## Created: 2020-04-18 (74bc503)
##
## Updated: 2023-10-04
##
## Copyright (c) Jason Ratcliff, 2023
##
## =============================================================================
##
## A SEINet search was performed using the following terms for
## "Taxonomic Criteria" of `Scientific Name` with Synonyms included:
##
##   * Physaria floribunda subsp. floribunda
##   * Physaria bellii
##   * Physaria rollinsii
##   * Physaria floribunda subsp. osterhoutii
##
##  Citation:
##  SEINet Portal Network. 2023. http//:swbiodiversity.org/seinet/index.php.
##      Accessed on September 09.
##
## =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(fs)
  library(lubridate)
  library(purrr)
  library(readr)
  library(tibble)
  library(usethis)
  library(thesis)
})

## ---- read-intersection ------------------------------------------------------

# Read in raw SEINet occurrence data limited to `vouchers` column intersection.
occurrences <- readr::read_csv(
  file = fs::path("data-raw/seinet/occurrences", ext = "csv"),
  col_select = c(
    dplyr::any_of(names(vouchers)),
    dplyr::matches(c("rights", "recordID", "^references$"))
  )
)

# stopifnot(all(names(occurrences) %in% names(vouchers)))

## ---- cast-occurrences -------------------------------------------------------

intersection <- intersect(names(occurrences), names(vouchers))

# Cast SEINet data to match classes in herbarium vouchers to facilitate join.
seinet <- purrr::pmap_dfc(
  .l = list(
    .voucher = vouchers[, intersection],
    .seinet = occurrences[, intersection],
    .name = names(occurrences[, intersection])
  ),
  .f = function(.voucher, .seinet, .name) {
    cast <- switch(
      class(.voucher)[1],
      character = as.character(.seinet),
      numeric = as.numeric(.seinet),
      integer = as.integer(.seinet),
      POSIXct = try(
        lubridate::parse_date_time(.seinet, orders = c("Ymd", "Ym", "Y"))
      ),
      .seinet
    )
    tibble::tibble({{.name}} := cast)
    }
  ) %>%
  dplyr::mutate(
    scientificName = stringr::str_replace(
      string = .data$scientificName,
      pattern = "(var\\.)|(subsp(?!\\.))",
      replacement = "subsp."
    ),
    scientificName = purrr::map_chr(
      .x = .data$scientificName,
      .f = ~ dplyr::case_when(
        identical(.x, "Physaria floribunda") ~
          "Physaria floribunda subsp. floribunda",
        identical(.x, "Physaria osterhoutii") ~
          "Physaria floribunda subsp. osterhoutii",
        TRUE ~ .x
      )
    )
  )

## ---- data-licenses ----------------------------------------------------------

# Include license data for individual specimen records.
seinet <-
  dplyr::bind_cols(
    seinet,
    dplyr::select(.data = occurrences, "rights":"references")
  )

## ---- filter-coordinates -----------------------------------------------------

# Filter records to exclude any missing geographic coordinates.
seinet <- seinet %>%
  dplyr::filter(
    !is.na(decimalLongitude),
    !is.na(decimalLatitude)
  )

## ---- write-seinet -----------------------------------------------------------

usethis::use_data(seinet, overwrite = TRUE)
