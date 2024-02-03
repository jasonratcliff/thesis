## =============================================================================
##
## Title: Herbarium Voucher R Data
##
## Author: jasonratcliff
##
## Created: 2020-02-22 (31cb433)
##
## Updated: 2023-09-07
##
## Copyright (c) Jason Ratcliff, 2023
##
## =============================================================================
##
## Notes:
##
## Herbarium voucher and field specimen data were recorded into a `.xlsx`
## file containing collection metadata and morphological trait observations.
##
## This script combines records from multiple sheets, each containing a
## particular species and / or suspected infraspecific taxa, into a tibble
## class data frame. The processed data is exported as an R object `vouchers`
## from the `thesis` package namespace from raw data as the project path:
##
## > `data-raw/specimens/vouchers.xlsx`
##
## Here, information from transcribed collection labels and field notes are
## standardized to Darwin Core terms for describing biodiversity information.
##
## Dates:
##
## In the herbarium specimen .xlsx file, dates were converted to an
## Excel Date format "mm/dd/yyyy" (e.g. 06/15/2003) and saved to a new column
## using the expression '=TEXT(<CELL>, "mm/dd/yyyy")'.  That column was copied
## and saved to a new column using 'paste special...' by value.
##
## =============================================================================

## ---- packages --------

suppressPackageStartupMessages({
  library(dplyr)
  library(fs)
  library(glue)
  library(lubridate)
  library(purrr)
  library(readxl)
  library(stringr)
  library(tibble)
  library(usethis)
  library(thesis)
})

## ---- vouchers-xlsx ----------------------------------------------------------

# Map separate species sheet names to row-bind tibble data frame from .xlsx file
voucher <- list(xlsx = fs::path("data-raw/specimens/vouchers", ext = "xlsx"))
voucher$specimens <- readxl::excel_sheets(path = voucher$xlsx) %>%
  purrr::keep(.p = ~ !grepl(pattern = "excluded", x = .x)) %>%
  purrr::map_dfr(.f = function(sheet) {
    readxl::read_xlsx(
      path = voucher$xlsx,
      sheet = sheet,
      na = c("", "NA", "s.n."),
      col_types = "text"
    ) %>%
      tibble::add_column(datasetID = sheet, .before = TRUE)
  }) %>%
  tibble::rowid_to_column(var = "collectionID")

## ---- voucher-records --------------------------------------------------------

dwcRecord <- voucher$specimens %>%
  dplyr::select(collectionID, datasetID, Herbarium) %>%
  dplyr::rename(institutionCode = Herbarium)

## ---- reviewed-annotations ---------------------------------------------------

# Replace instances of `ssp.` with `subsp.` in reviewed annotations
dwcTaxon <- voucher$specimens %>%
  dplyr::select(Taxon_a_posteriori) %>%
  dplyr::mutate(
    scientificName = gsub(
      pattern = "ssp.", replacement = "subsp.",
      x = .data$Taxon_a_posteriori
    ),
    .keep = "unused"
  )

## ---- voucher-occurrences ----------------------------------------------------

dwcOccurrence <- voucher$specimens %>%
  dplyr::select(
    Collector, Collection_Number,
    Association, Special_Note
  ) %>%
  dplyr::rename(
    recordedBy = Collector,
    recordNumber = Collection_Number,
    associatedTaxa = Association,
    occurrenceRemarks = Special_Note
  )

## ---- collection-dates -------------------------------------------------------

# Event: Extract fields related to the time of specimen collection.
dwcEvent <- voucher$specimens %>%
  dplyr::select(Date, Description) %>%
  dplyr::mutate(
    .keep = "unused", .before = "Description",
    verbatimEventDate = Date,
    eventDate = lubridate::parse_date_time(
      x = .data$verbatimEventDate,
      orders = c("mdY", "md", "Y")
    ),
    year = lubridate::year(.data$eventDate),
    month = lubridate::month(.data$eventDate),
    day = lubridate::day(.data$eventDate)
  ) %>%
  dplyr::rename(habitat = Description)

## ---- prior-identifications --------------------------------------------------

# Identifications:
dwcIdentification <- voucher$specimens %>%
  dplyr::mutate(
    verbatimIdentification = Taxon,
    typeStatus = Type,
    .keep = "none"
  )

# Organism: Process fields linking taxonomic definitions to occurrence records.
dwcOrganism <- dwcIdentification %>%
  dplyr::mutate(
    # Map prior identifications to handle author names and concurrence IDs.
    previousIdentifications = purrr::map(
      .x = verbatimIdentification,
      .f = function(id) {
        annotations <- stringr::str_split(string = id, pattern = ", ?")[[1]]

        # Extract species name ± subspecies rank.
        identifications <- purrr::map_chr(
          .x = annotations,
          .f = function(sp) {
            if (grepl("!", x = sp)) {
              return(sp)
            }
            species <-
              stringr::str_extract(sp, pattern = "^([A-z]+)( ?[A-z]+)?")
            subspecies <-
              stringr::str_extract(sp, pattern = " (ssp|subsp|var).? [a-z]+")
            glue::glue(species, subspecies, .na = "")
          }
        )

        # Replace "!" annotation concurrence with species name.
        purrr::imap_chr(
          .x = identifications,
          .f = function(sp, i) {
            if (grepl("!", sp)) {
              identifications[i] <<- identifications[i - 1]
              return(identifications[i])
            }
            return(sp)
          }
        ) %>%
          paste0(collapse = " | ")
      }
    ),

    # Account for taxonomic synonyms in most recent identification.
    organismName = purrr::map_chr(
      .x = previousIdentifications,
      .f = function(ids) {
        annotations <- stringr::str_split(string = ids, pattern = " \\| ")[[1]]
        id <- # Add missing periods for taxonomic ranks
          stringr::str_replace(
            string = annotations[length(annotations)],
            pattern = "(?<=(ssp|var)) ", replacement = ". "
          )
      }
    ),

    # Check for (sub-)species synonyms and replace with updated taxon.
    organismName = purrr::map_chr(
      .x = organismName,
      .f = function(id) {
        # Early escape hatch to avoid checking for string replacements.
        if (id %in% thesis::aesthetics$species) {
          return(id)
        }
        dplyr::case_when(
          grepl(pattern = "australis|purpurea|stylosa", x = id) ~
            "Physaria acutifolia",
          grepl(pattern = "integrifolia|monticola", x = id) ~
            "Physaria integrifolia",
          grepl(pattern = "didymocarpa|lanata", x = id) ~
            dplyr::case_when(
              grepl(pattern = "lanata", x = id) ~
                "Physaria didymocarpa subsp. lanata",
              grepl(pattern = "lyrata", x = id) ~
                "Physaria didymocarpa subsp. lyrata",
              TRUE ~ "Physaria didymocarpa subsp. didymocarpa"
            ),
          grepl(pattern = "saximontana|dentata", x = id) ~
            dplyr::case_when(
              grepl(pattern = "dentata", x = id) ~
                "Physaria saximontana subsp. dentata",
              TRUE ~ "Physaria saximontana subsp. saximontana"
            ),
          TRUE ~ id
        )
      }
    )
  ) %>%
  dplyr::select(organismName, previousIdentifications)

## ---- voucher-locations ------------------------------------------------------

# Convert geographic coordinate column classes from character to numeric.
dwcLocation <- voucher$specimens %>%
  dplyr::select(State, County, Location, Latitude, Longitude) %>%
  dplyr::mutate(
    dplyr::across(
      .cols = dplyr::all_of(c("Latitude", "Longitude")),
      .fns = \(x) as.numeric(x)
    )
  ) %>%
  dplyr::rename_with(
    .cols = dplyr::where(is.numeric),
    .fn = \(x) glue::glue("decimal{x}")
  ) %>%
  dplyr::rename(
    stateProvince = State,
    county = County,
    verbatimLocality = Location
  )

## ---- voucher-elevations -----------------------------------------------------

dwcElevation <- voucher$specimens %>%
  dplyr::select(dplyr::matches(match = "^Elev_\\((m|ft\\.)\\)")) %>%
  dplyr::rename_with(
    .fn = ~ stringr::str_remove_all(string = .x, pattern = "[^[A-z]_]")
  ) %>%
  dplyr::mutate(
    # Concatenate raw elevation observations across units
    verbatimElevation = unclass(glue::glue("{Elev_m}, {Elev_ft}")),

    # Process collection elevation records from meter and feet vectors
    dplyr::across(
      .cols = dplyr::matches("^Elev_"),
      .fns = function(observed) {
        # Exclude non-standard elevations; expect (± ranged) character data
        elevation <- stringr::str_squish(string = observed)
        processed <- dplyr::case_when(
          grepl(pattern = "UTM|sn\\.", x = elevation) ~ NA_character_,
          stringr::str_detect(string = elevation, pattern = "[^[0-9]-.]") ~
            stringr::str_remove_all(string = elevation, pattern = "[^[0-9]-\\.]"),
          TRUE ~ elevation
        )

        # Split ranged data from processed strings, handling NA values
        separated <- stringr::str_split(processed, pattern = "-")

        # Return list column of double vectors cast from split strings
        purrr::map(.x = separated, .f = ~ as.numeric(.x))
      }
    ),

    # Combine range-separated values, convert ft. to m, and round to nearest m
    converted = purrr::map2(
      .x = .data$Elev_m, .y = .data$Elev_ft,
      .f = function(m, ft) {
        sorted <- sort(c(m, ft / 3.281))
        if (length(sorted) == 0) {
          return(NA_real_)
        }
        round(sorted)
      }
    ),

    # Calculate minimum / maximum for non-identical values
    minimumElevationInMeters = purrr::map_dbl(.x = converted, .f = ~ min(.x)),
    maximumElevationInMeters = purrr::map2_dbl(
      .x = converted, .y = minimumElevationInMeters,
      .f = function(x, y) {
        dplyr::case_when(
          length(x) == 1 ~ NA_real_,
          max(x) != y ~ max(x),
          TRUE ~ NA_real_
        )
      }
    )
  ) %>%
  dplyr::select(minimumElevationInMeters, maximumElevationInMeters, verbatimElevation)

## ---- trait-data -------------------------------------------------------------

dwcTraits <- voucher$specimens %>%
  dplyr::select(Rosulate:`Chromosome #`) %>%
  dplyr::rename(Chromosome = "Chromosome #", Ovule_n = "Ovule_number") %>%
  dplyr::rename_with(
    .fn = ~ purrr::map_chr(
      .x = .,
      .f = function(trait) {
        split <- stringr::str_split(trait, pattern = "_")[[1]]
        quantitative <-
          grepl(pattern = "[mcd]m|n$", x = split[length(split)])
        joined <- dplyr::if_else(
          condition = quantitative,
          true = glue::glue_collapse(
            c(
              split[length(split)],
              stringr::str_to_title(split[1:(length(split)-1)])
            )
          ),
          false = glue::glue_collapse(stringr::str_to_title(split))
        )
      }
    )
  )

## ---- write-vouchers ---------------------------------------------------------

vouchers <-
  dplyr::bind_cols(
    dwcRecord, dwcTaxon, dwcOrganism, dwcOccurrence,
    dwcEvent, dwcLocation, dwcElevation, dwcIdentification,
    dwcTraits
  )

usethis::use_data(vouchers, overwrite = TRUE)
