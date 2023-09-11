## =============================================================================
##
## Title: Herbarium Voucher R Data
##
## Author: jasonratcliff
##
## Created: 2020-22-02 (31cb433)
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
})

## ---- vouchers-xlsx --------

# Map separate species sheet names to row-bind tibble data frame from .xlsx file
voucher <- list(xlsx = fs::path("data-raw/specimens/vouchers", ext = "xlsx"))
voucher$specimens <- readxl::excel_sheets(path = voucher$xlsx) %>%
  purrr::keep(.p = ~ !grepl(pattern = "excluded", x = .x)) %>%
  purrr::map_dfr(.f = function(sheet) {
    readxl::read_xlsx(
      path = voucher$xlsx,
      sheet = sheet,
      na = c("", "NA", "s.n."),
      col_types = c(
        # Skip uninformative columns in species sheets raw data
        rep("text", 16), rep("skip", 10), rep("text", 2), rep("skip", 1),
        rep("text", 36) # Morphological trait observations (AD:DM)
      )
    ) %>%
      tibble::add_column(datasetName = sheet, .before = TRUE)
  }) %>%
  # Retain records matching study-relevant genus / family names
  dplyr::filter(
    grepl(
      pattern = "Physaria|Lesquerella|Brassicaceae",
      x = .data$Taxon, ignore.case = TRUE
    )
  ) %>%
  # Add unique identifier for collection records in data set
  tibble::rowid_to_column(var = "catalogNumber")

specimens <- list()
specimens$raw <-
  # Parse dates with lubridate, create vector for month / day and reorder.
  dplyr::mutate(
    Date_parsed = lubridate::mdy(Date),
    Date_md = gsub(
      pattern = "/[0-9]{4}", "",
      x = .data$Date
    ) %>%
      as.Date(
        x = .,
        format = "%m/%d"
      )
  ) %>%
  dplyr::select(
    excel_sheet:Collection_Number,
    dplyr::matches("Date"),
    Herbarium:`Chromosome #`
  ) %>%
  dplyr::rename(Chromosomes = "Chromosome #")

# Log Date Formats ----
#
# In the herbarium specimen .xlsx file, dates were converted to an
# Excel Date format "mm/dd/yyyy" (e.g. 06/15/2003) and saved to a new column
# using the expression '=TEXT(<CELL>, "mm/dd/yyyy")'.  That column was copied
# and saved to a new column using 'paste special...' by value.
#
# Check for log directory, create if non-existent, and open log file.
if (!dir.exists("inst/log")) {
  dir.create("inst/log")
}

# Select date columns, filter by format, and write mismatches to a .csv log.
specimens$raw %>%
  dplyr::select(
    excel_sheet, Collector, Collection_Number, Date,
    Date_parsed, ID
  ) %>%
  dplyr::filter(
    .,
    !grepl(
      pattern = "[1-2][0-9][0-9][0-9]-[0-1][0-9]-[0-3][0-9]",
      x = as.character(Date_parsed)
    ) & !is.na(Date) |
      !grepl(pattern = "[!C\\?] [0-1][0-9]/[0-3][0-9]/[1][5-9]", x = ID)
  ) %>%
  readr::write_excel_csv(x = ., file = "inst/log/remaining_dates.csv")

# Parse Prior ID ----

# Function to recursively replace identification agreements marked by "!"
prior_ids <- function(prior_vector) {
  # Detect first index of ID agreement and replace with previous ID.
  if (TRUE %in% grepl(pattern = "!", x = prior_vector)) {
    id_match <- grep(pattern = "!", x = prior_vector) %>% min()
    prior_vector[id_match] <- prior_vector[id_match - 1]
    return(prior_ids(prior_vector))
  } else {
    return(prior_vector)
  }
}

# Built tibble from prior annotation history.
specimens$split <- specimens$raw$Taxon %>%
  # Map list of comma-separated annotations to replace synonyms and concurrence.
  stringr::str_split(string = ., pattern = ", ?") %>%
  purrr::map(.x = ., function(split_ids) {
    prior_ids(prior_vector = split_ids)
  })

specimens$priors <- specimens$split %>%
  # Subset most recent identification from list of annotation vectors.
  purrr::map_chr(.x = ., .f = function(annotations) {
    annotations[length(annotations)]
  }) %>%
  # Remove author names and replace variety with subsp. abbreviations.
  stringr::str_replace_all(
    string = ., replacement = "",
    pattern = " \\((Payson|Hook\\.)\\)| Gray| A\\.| Hitch\\.| Rollins"
  ) %>%
  stringr::str_replace_all(
    string = .,
    pattern = "var\\.?|var\\.$|(ssp|subsp).?(?= )", replacement = "subsp."
  ) %>%
  # Replace identification synonyms.
  ifelse(grepl("australis|purpurea|stylosa", x = .),
    yes = "Physaria acutifolia", no = .
  ) %>%
  ifelse(grepl("integrifolia", x = .),
    yes = "Physaria integrifolia", no = .
  ) %>%
  ifelse(grepl("Physaria didymocarpa( ssp\\.$)?$|normalis", x = .),
    yes = "Physaria didymocarpa subsp. didymocarpa", no = .
  ) %>%
  ifelse(grepl("lanata", x = .),
    yes = "Physaria didymocarpa subsp. lanata", no = .
  ) %>%
  ifelse(grepl("Physaria saximontana$", x = .),
    yes = "Physaria saximontana subsp. saximontana", no = .
  ) %>%
  # Enframe character vector as tibble.
  tibble::enframe(value = "prior_id", name = NULL) %>%
  # Combine recent ID and
  dplyr::bind_cols(
    plyr::ldply(
      .data = specimens$split,
      .fun = rbind
    ) %>%
      setNames(
        object = .,
        nm = paste0("prior_", names(.))
      )
  )

# Replace instances of `ssp.` with `subsp.` in reviewed annotations
specimens$raw$Taxon_a_posteriori <- specimens$raw$Taxon_a_posteriori %>%
  gsub(pattern = "ssp.", "subsp.", x = .)

# Parse Elevation ----

# Convert geographic coordinate column classes from character to numeric.
specimens$geography <- specimens$raw %>%
  dplyr::select(Longitude, Latitude) %>%
  dplyr::mutate_all(.tbl = ., ~ as.numeric(.x))

## ---- voucher-elevation --------

elevations <- specimens$raw %>%
  dplyr::select(dplyr::matches(match = "^Elev_\\((m|ft\\.)\\)")) %>%
  dplyr::rename_with(
    .fn = ~ stringr::str_remove_all(string = .x, pattern = "[^[A-z]_]")
  ) %>%
  dplyr::mutate(
    # Concatenate raw elevation observations across units
    verbatimElevation = glue::glue("{Elev_m}, {Elev_ft}"),

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
        separated <- tryCatch(
          stringr::str_split(processed, pattern = "-"),
          error = function(e) processed
        )

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


# Bind Columns ----

vouchers <-
  dplyr::bind_cols(
    datasetName = specimens$raw$datasetName,
    specimens$priors,
    dplyr::select(specimens$raw, Taxon:Location),
    specimens$geography,
    dplyr::select(specimens$raw, ID:Imaged),
    elevations,
    dplyr::select(specimens$raw, TRS1:Chromosomes)
  )

# Write Data .Rda ----

usethis::use_data(vouchers, overwrite = TRUE)
