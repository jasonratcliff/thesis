library(magrittr)
library(readxl)
library(readr)
library(plyr)
library(dplyr)
library(purrr)
library(lubridate)
library(stringr)
library(tibble)
library(ThesisPackage)

if (basename(getwd()) != "ThesisPackage") {
  stop("Source from top level of `ThesisPackage`.")
}

# 1. Read in specimen data ----
#
# An .xlsx file located in the `data-raw/` subdirectory contains
# specimen voucher information from project herbarium records, including
# geographic coordinate data (decimal degrees), collection dates,
# annotation information, trait measurements and observations.

# Map .xlsx sheetnames to read tibbles from .xlsx file..
specimen_excel_path <- system.file("extdata/specimens.xlsx",
                                   package = "ThesisPackage")
specimens_raw <-
  readxl::excel_sheets(path = specimen_excel_path) %>%
  purrr::map(function(excel_sheet) {
    readxl::read_xlsx(path = specimen_excel_path,
                      sheet = excel_sheet, na = c("", "NA", "s.n."),
                      col_types = c(rep("text", 16), rep("skip", 10),
                                    rep("text", 2), rep("skip", 1),
                                    rep("text", 36))) %>%
      tibble::add_column(excel_sheet = excel_sheet, .before = TRUE)
    }) %>%

  # bind row-wise and remove rows not matching Genera / Family of interest.
  dplyr::bind_rows() %>%
  dplyr::filter(grepl("Physaria|Lesquerella|Brassicaceae", Taxon)) %>%

  # Parse dates with lubridate, create vector for month / day and reorder.
  dplyr::mutate(Date_parsed = lubridate::mdy(Date)) %>%
  dplyr::mutate(Date_md = gsub("/[0-9]{4}", "", x = Date) %>%
                  as.Date(., format = "%m/%d")) %>%
  dplyr::select(excel_sheet:Date, Date_parsed, Date_md,
                Herbarium:`Chromosome #`) %>%
  dplyr::rename(Chromosomes = "Chromosome #")

# 2. Log collection and ID date formats ----
#
# In the herbarium specimen .xlsx file, dates were converted to an
# Excel Date format "mm/dd/yyyy" (e.g. 06/15/2003) and saved to a new column
# using the expression '=TEXT(<CELL>, "mm/dd/yyyy")'.  That column was copied
# and saved to a new column using 'paste special...' by value.
#
# Check for log directory, create if non-existent, and open log file.
if (!dir.exists("log")) {
  dir.create("log")
}

# Select date columns, filter by format, and write mismatches to a .csv log.
specimens_raw %>%
  dplyr::select(excel_sheet, Collector, Collection_Number, Date,
                Date_parsed, ID) %>%
  dplyr::filter(.,
    !grepl(pattern = "[1-2][0-9][0-9][0-9]-[0-1][0-9]-[0-3][0-9]",
           x = as.character(Date_parsed)) & !is.na(Date) |
      !grepl(pattern = "[!C\\?] [0-1][0-9]/[0-3][0-9]/[1][5-9]", x = ID)) %>%
  readr::write_excel_csv(x = ., file = "log/remaining_dates.csv")

# 3. Parse prior identifications ----

# Function to recursively replace identification agreements marked by "!"
prior_ids <- function(prior_vector) {
  # Detect first index of ID agreement and replace with previous ID.
  if (TRUE %in% grepl(pattern = "!", x = prior_vector)) {
    id_match <- grep(pattern = "!", x = prior_vector) %>% min()
    prior_vector[id_match] <- prior_vector[id_match - 1]
    return(prior_ids(prior_vector))  # Recursive function call.
    } else {
      return(prior_vector)
    }
}

# Assign list of prior specimens annotations accounting for "!" ID's.
specimens_split <- specimens_raw %>% dplyr::pull(Taxon) %>%
  strsplit(x = ., split = ", ") %>%
  purrr::map(function(split_ids) prior_ids(prior_vector = split_ids))

# Assign tibble column from character vector with most recent annotations.
specimens_recent <- specimens_split %>%
  purrr::map_chr(function(annotations) {
    annotations[length(annotations)]
  }) %>%
  # Remove author names and replace variety with subsp. abbreviations.
  stringr::str_replace_all(string = ., replacement = "",
    pattern =  " \\((Payson|Hook\\.)\\)| Gray| A\\.| Hitch\\.| Rollins") %>%
  stringr::str_replace_all(string = .,
    pattern = "var\\.?|var\\.$|ssp(?= )", replacement = "ssp.") %>%

  # Replace identification synonyms.
  ifelse(grepl("australis|purpurea|stylosa", x = .),
         yes = "Physaria acutifolia", no = .) %>%
  ifelse(grepl("integrifolia", x = .),
         yes = "Physaria integrifolia", no = .) %>%
  ifelse(grepl("Physaria didymocarpa( ssp\\.$)?$|normalis", x = .),
         yes = "Physaria didymocarpa ssp. didymocarpa", no = .) %>%
  ifelse(grepl("lanata", x = .),
         yes = "Physaria didymocarpa ssp. lanata", no = .) %>%
  ifelse(grepl("Physaria saximontana$", x = .),
         yes = "Physaria saximontana ssp. saximontana", no = .) %>%

  # Enframe character vector as tibble.
  tibble::enframe(value = "prior_id", name = NULL)

# Combine recent ID, split IDs, and raw herbarium record info.
specimens_parsed <- dplyr::bind_cols(specimens_raw %>%
  dplyr::select(excel_sheet), specimens_recent,
  plyr::ldply(.data = specimens_split, rbind) %>%
    stats::setNames(object = ., paste0("prior_", names(.))) %>%
    tibble::as_tibble(),
  specimens_raw %>% dplyr::select(Taxon:Chromosomes))

# Clean up workspace.
rm(specimens_raw, specimens_split, specimens_recent, prior_ids)

# 4. Parse elevation data ----

# Convert geographic coordinate column classes from character to numeric.
specimens_parsed <- specimens_parsed %>%
  dplyr::mutate(Longitude = as.numeric(Longitude)) %>%
  dplyr::mutate(Latitude = as.numeric(Latitude)) %>%

# Rename columns, remove illegal characters, and replace NA elevation data.
  dplyr::rename(Elev_ft = `Elev_(ft.)`, Elev_m = `Elev_(m)`) %>%
  dplyr::mutate(Elev_ft = gsub(",|'|~", "", x = Elev_ft) %>%
                  gsub("[A-Za-z].+", NA, x = .) %>% gsub(" +", "", x = .),
                Elev_m = gsub(",|'|~", "", x = Elev_m) %>%
                  gsub("[A-Za-z].+", NA, x = .) %>% gsub(" +", "", x = .))

#  Map tibble data frame to merge ft / m elevation data.
elev_parsed <-
  specimens_parsed %>% dplyr::select(Elev_ft, Elev_m) %>%
  purrr::pmap_dfr(function(Elev_ft, Elev_m) {
    if (is.na(Elev_ft) & !is.na(Elev_m)) {
      dplyr::bind_cols(Elev_var = "Elev_m", Elev_raw = Elev_m)
      } else {
        dplyr::bind_cols(Elev_var = "Elev_ft", Elev_raw = Elev_ft)
        }
    })

# Map tibble data frame with split range data and convert meters to ft.
elev_parsed <-
  dplyr::bind_cols(elev_parsed,
    ThesisPackage::range_split(trait_tbl = elev_parsed,
                               split_var = "Elev_raw")) %>%
  purrr::pmap_dfr(.l = ., function(Elev_var, Elev_raw_min,
                                   Elev_raw_max, Elev_raw) {
    if (Elev_var == "Elev_m") {
      elev_min <- as.numeric(Elev_raw_min) * 3.281
      elev_max <- as.numeric(Elev_raw_max) * 3.281
      dplyr::bind_cols(Elev_var = Elev_var, Elev_raw = Elev_raw,
                       Elev_raw_min = elev_min, Elev_raw_max = elev_max)
      } else {
        dplyr::bind_cols(Elev_var = Elev_var, Elev_raw = Elev_raw,
          Elev_raw_min = Elev_raw_min, Elev_raw_max = Elev_raw_max)
        }
    })

# Bind parsed specimens tibble with merged elevation vectors.
herbarium_specimens <-
  dplyr::bind_cols(dplyr::select(specimens_parsed, excel_sheet:Elev_ft),
    elev_parsed, dplyr::select(specimens_parsed, TRS1:Chromosomes))

rm(elev_parsed, specimens_parsed) # Clean up workspace

# 5. Bind DNA Extraction Information ----

# Combine herbarium specimen record information with sequence documentation.
dna_metadata <-
  readr::read_csv(file = "data-raw/specimens/dna_specimens.csv",
                  col_types = "ccdcccddcclccc") %>%
  dplyr::rename(label = taxa_label)

dna_specimens <- purrr::pmap_dfr(dna_metadata,
  function(label, Collector, Collection_Number, ...) {

    # Subset total herbarium record data frame by collector and collection.
    record_match <- herbarium_specimens %>%
      dplyr::filter(., Collector == !!Collector &
                      Collection_Number == !!Collection_Number)  %>%
      dplyr::mutate(Collection_Number = as.numeric(Collection_Number)) %>%
      dplyr::select(-c(State, County))

    # Join the herbarium records to matching sequencing specimen .csv rows.
    dplyr::left_join(dplyr::filter(dna_metadata, label == !!label),
      record_match, by = c("Collector", "Collection_Number"))
    }) %>%

  # Account for duplicate label matches from joining.
  dplyr::group_by(.data$label) %>% dplyr::slice(1) %>% dplyr::ungroup()

rm(dna_metadata) # Clean up workspace

# 6. .Rda files ----
usethis::use_data(herbarium_specimens, overwrite = TRUE)
usethis::use_data(dna_specimens, overwrite = TRUE)

# Define subset from ThesisPackage::herbarium_specimens
# TODO Move to herbarium_specimens.R and rename Roxygen examples and tests.

# Roxygen Example Dataset
spp_co_front_range <-
  subset_coords(herbarium_specimens,
                Longitude = c(-107.5, -104.5),
                Latitude = c(38, 41))

