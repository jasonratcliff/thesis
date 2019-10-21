#!/usr/bin/env Rscript

library(magrittr)

# Catch command line arguments.
args <- commandArgs(trailingOnly = TRUE)
species_sheet <- as.character(args[1])

#' Appendix R Script
#'
#' Function to write an ordered .tsv file from a given sheet name of the .xlsx
#' file "Phys_species_totals.xlsx" in the `data/1.specimens`` subdirectory.
#'
#' @param species_tab: Character vector of length 1 to match .xlsx sheetname.
species_appendix <- function(species_tab) {

  # Verify sheet name matches in .xlsx file.
  specimen_file <- "data/1.specimens/Phys_species_totals.xlsx"
  phys_total_sheets <- readxl::excel_sheets(specimen_file)
  if ((TRUE %in% unique(grepl(paste0("^", species_tab, "$"),
                              phys_total_sheets))) == FALSE) {
    stop(paste("Variable SPECIES_TAB must match sheet name exactly...\n",
               "check variable: ", species_tab))
  }

  # Read in specimen data by sheet name and order columns for appendix style.
  app_names <- c("Taxon", "Location", "Elev_(ft.)", "Elev_(m)", "TRS1", "TRS2",
                 "Date", "Collector", "Collection_Number", "Herbarium", "App.A")
  xl_names <-
    readxl::read_excel(specimen_file, 
                       range = readxl::cell_rows(1)) %>% names()
  app_cols <- which(xl_names %in% app_names)
  app_tbl <- readxl::read_excel(path = specimen_file, sheet = species_tab,
                                range = readxl::cell_cols(app_cols)) %>%
    dplyr::select(app_names) %>%  # Rearrange tibble by appendix names vector.
    dplyr::filter(grepl("Physaria|Lesquerella|Brassicaceae", Taxon)) %>%
    dplyr::filter(is.na(.[["App.A"]]))  %>% # Filter rows missing from appendix.
    dplyr::select(-Taxon)

  # Write .tsv file from ordered tibble for remaining appendix samples.
  app_file <- paste0("Appendix/", gsub(" +", "", species_tab),
                              "_appendix.tsv")
  cat(app_file)
  write.table(x = app_tbl, file = app_file, 
              sep = "\t", eol = "\n", quote = FALSE)
}

species_appendix(species_sheet)

