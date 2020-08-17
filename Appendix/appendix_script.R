#!/usr/bin/env Rscript
library(magrittr)

#' Appendix R Script
#'
#' Write ordered .tsv files from .xlsx file sheet names.
#' Column variables are limited to those included in specimen appendix pages.
#' Installed file "specimens.xlsx" contains voucher data describing collection.
#'
#' @param species_tab: Character vector of length 1 to match .xlsx sheetname.
#'
species_appendix <- function(species_tab) {

  # Read in specimen data by sheet name and order columns for appendix style.
  specimens <-
    system.file("extdata/specimens.xlsx", package = "ThesisPackage") %>%
    readxl::read_excel( path = ., sheet = species_tab) %>%
    dplyr::select(
      dplyr::one_of(c("Taxon", "Location", "Elev_(ft.)", "Elev_(m)",
                      "TRS1", "TRS2", "Date", "Collector", "Collection_Number",
                      "Herbarium", "App.A"))
    ) %>%
    dplyr::rename_with(., ~ gsub("\\(|\\)", "", .x) %>%
                         gsub("\\.$", "", .)) %>%
    dplyr::filter(
      grepl(pattern = "Physaria|Lesquerella|Brassicaceae",
            x = Taxon) & is.na(.[["App.A"]])
      ) %>% dplyr::select(-Taxon)

  # Write .tsv file from ordered tibble for remaining appendix samples.
  readr::write_tsv(
    x = specimens,
    path =  paste0("Appendix/", gsub(" +", "", species_tab), "_appendix.tsv")
  )

}

# Write .tsv files with selected appendix entry columns.
readxl::excel_sheets(
  path = system.file("extdata/specimens.xlsx", package = "ThesisPackage")
) %>% purrr::walk(.x = ., ~ species_appendix(species_tab = .))

# Walk appendix files to cleanup formatting and write new .tsv files.
purrr::walk(
  .x = list.files(path = "Appendix", pattern = "_appendix.tsv",
                  full.names = TRUE),
  function(appendix_sheet) {

    appendix_entry <- readr::read_tsv(
      file = appendix_sheet,
      col_types = paste0(rep("c", times = 10), collapse = "")) %>%

      dplyr::mutate(
        count_id = dplyr::row_number(),

        # TODO Abstract out mutate calls - possible to simplify?
        Elev_ft = purrr::map_chr(
          .x = .data$Elev_ft,
          function(x) ifelse(is.na(x),
                             yes = NA, no = paste0(x, "' elevation;"))),
        Elev_m = purrr::map_chr(
          .x = .data$Elev_m,
          function(x) ifelse(is.na(x),
                             yes = NA, no = paste0(x, "m elevation;"))),
        Collector = purrr::map_chr(
          .x = .data$Collector,
          function(x) ifelse(is.na(x),
                             yes = NA, no = paste0("\\textit{", x))),
        Collection_Number = purrr::map_chr(
          .x = .data$Collection_Number,
          function(x) ifelse(is.na(x),
                             yes = NA, no = paste0(x, "}"))),
        TRS2 = purrr::map_chr(
          .x = .data$TRS2,
          function(x) ifelse(is.na(x),
                             yes = NA, no = paste0(x, ";"))),
        Date = purrr::map_chr(
          .x = .data$Date,
          function(x) ifelse(is.na(x),
                             yes = NA, no = paste0(x, ";")))
        ) %>%
      dplyr::select("count_id", dplyr::everything(), -c("App.A")) %>%

      # Unite character vector from tibble columns and clean NA / whitespace.
      tidyr::unite(data = ., col = "Appendix", sep = " ") %>%
      dplyr::mutate(
        Appendix = gsub(pattern = "NA", replacement =  "",
                        x = .data$Appendix) %>%
          gsub(pattern = " +", replacement = " ", x = .)
      )

    # Write new file with formatted specimen appendix entries.
    if (nrow(appendix_entry) > 0) {
      readr::write_lines(
        x = appendix_entry$Appendix, sep = "\n",
        fs::path(
          paste0(fs::path_ext_remove(appendix_sheet), "_formatted"),
          ext = "tsv")
      )
    }
    fs::file_delete(appendix_sheet)  # remove unformatted file
  })

