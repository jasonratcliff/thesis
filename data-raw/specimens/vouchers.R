library(magrittr)
library(readxl)
library(readr)
library(plyr)
library(dplyr)
library(purrr)
library(lubridate)
library(stringr)
library(tibble)
library(thesis)

if (basename(getwd()) != "thesis") {
  stop("Source from top level of `thesis`.")
}

# Read Specimens ----
#
# An .xlsx file located in the `data-raw/` subdirectory contains
# specimen voucher information from project herbarium records, including
# geographic coordinate data (decimal degrees), collection dates,
# annotation information, trait measurements and observations.

specimens <- list()

# Map .xlsx sheetnames to read tibbles from .xlsx file..
specimens$path <-
  fs::path(here::here(), "data-raw/specimens/vouchers", ext = "xlsx")
specimens$raw <-
  readxl::excel_sheets(path = specimens$path) %>%
  purrr::keep(.x = ., ~ !grepl("excluded", x = .x)) %>%
  purrr::map_dfr(function(excel_sheet) {
    readxl::read_xlsx(
      path = specimens$path,
      sheet = excel_sheet,
      na = c("", "NA", "s.n."),
      col_types = c(
        rep("text", 16),
        rep("skip", 10),
        rep("text", 2),
        rep("skip", 1),
        rep("text", 36)
      )
    ) %>%
      tibble::add_column(excel_sheet = excel_sheet, .before = TRUE)
  }) %>%
  # bind row-wise and remove rows not matching Genera / Family of interest.
  dplyr::filter(
    grepl(
      pattern = "Physaria|Lesquerella|Brassicaceae",
      x = .data$Taxon
    )
  ) %>%
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

# Convert geographic coordinate column classes from character to numeric.
specimens$elevation <- specimens$raw %>%
  dplyr::select(dplyr::matches("Elev")) %>%
  dplyr::rename_at(
    .vars = dplyr::vars(dplyr::matches("Elev")),
    .funs = ~ paste0(
      "Elev_",
      stringr::str_extract(string = .x, pattern = "m|ft")
    )
  ) %>%
  dplyr::mutate_at(
    .vars = dplyr::vars(dplyr::matches("Elev")),
    .funs =
      ~ stringr::str_remove_all(
        string = .x,
        pattern = "[A-Za-z\\.,'~ ]"
      )
  ) %>%
  dplyr::mutate(
    Elev_var = dplyr::case_when(
      !is.na(Elev_ft) ~ "Elev_ft",
      !is.na(Elev_m) ~ "Elev_m",
      TRUE ~ NA_character_
    ),
    Elev_raw = dplyr::case_when(
      !is.na(Elev_ft) ~ as.character(Elev_ft),
      !is.na(Elev_m) ~ as.character(Elev_m),
      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::bind_cols(
    thesis::range_split(
      trait_tbl = .,
      split_var = "Elev_raw"
    )
  ) %>%
  dplyr::mutate(
    Elev_var = dplyr::case_when(
      !is.na(Elev_ft) ~ "Elev_ft",
      !is.na(Elev_m) ~ "Elev_m",
      TRUE ~ NA_character_
    ),
    Elev_raw = dplyr::case_when(
      !is.na(Elev_ft) ~ as.character(Elev_ft),
      !is.na(Elev_m) ~ as.character(Elev_m),
      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::mutate_at(
    .vars = dplyr::vars(dplyr::matches("Elev_raw_(min|max)")),
    .funs = ~ dplyr::case_when(
      Elev_var == "Elev_ft" ~ .x,
      Elev_var == "Elev_m" ~ .x * 3.281,
      TRUE ~ NA_real_
    )
  )

# Bind Columns ----

herbarium_specimens <-
  dplyr::bind_cols(
    excel_sheet = specimens$raw$excel_sheet,
    specimens$priors,
    dplyr::select(specimens$raw, Taxon:Location),
    specimens$geography,
    dplyr::select(specimens$raw, ID:Imaged),
    specimens$elevation,
    dplyr::select(specimens$raw, TRS1:Chromosomes)
  )

# Write Data .Rda ----

usethis::use_data(herbarium_specimens, overwrite = TRUE)
