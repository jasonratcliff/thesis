#!/usr/bin/env Rscript

# Appendix R Script
#
# Function to write a formatted .tsv file from a given sheet name in the .xlsx 
# file "Phys_species_totals.xlsx" in the project directory main level containing
# herbarium specimen data. Output is formatted for thesis appendix style.
#
# Args:
#   species_tab: Character vector of length 1 to exactly match .xlsx sheetname.

library(openxlsx)

# Catch command line arguments.
args <- commandArgs(trailingOnly = TRUE)
species_sheet <- as.character(args[1])

# print(args)

species_appendix <- function(species_tab) {
  
  # Verify sheet name matches in .xlsx file.
  phys_total_sheets <- openxlsx::getSheetNames("Phys_species_totals.xlsx")
  if ((TRUE %in% unique(grepl(paste0("^", species_tab, "$"), 
                              phys_total_sheets))) == FALSE) {
    stop(paste("Variable SPECIES_TAB must match sheet name exactly...\n",
               "check variable: ", species_tab))
  }
  
  # Read in specimen data by sheet name and order columns for appendix style.
  spec_col_names <- c("Location", "Elev_(ft.)", "Elev_(m)", "TRS1", "TRS2", 
                      "Date", "Collector", "Collection_Number", "Herbarium", 
                      "App.A")
  spec_row <- openxlsx::read.xlsx("Phys_species_totals.xlsx", 
                                  rows = 1, colNames = FALSE)
  spec_cols <- which(spec_row %in% spec_col_names)
  spec_app <- openxlsx::read.xlsx("Phys_species_totals.xlsx", colNames = TRUE,
                                  sheet = species_tab, cols = spec_cols)
  spec_col_order <- sapply(spec_col_names, USE.NAMES = FALSE,
                           FUN = function(name) {
                             grep(name, names(spec_app), fixed = TRUE)
                           })
  spec_app <- spec_app[, spec_col_order]
  
  # Subset specimen rows which have yet been added to an appendix .tex file.
  spec_app_subset <- spec_app[which(is.na(spec_app$App.A )), ]
  
  # Check directory and write table named by sheet tab.
  if (!dir.exists("Appendix")) {
    dir.create("Appendix/")
  }
  appendix_filename <- paste0("Appendix/", gsub(" +", "", species_tab), 
                              "_appendix.tsv")
  write.table(x = spec_app_subset, file = appendix_filename, 
              sep = "\t", eol = "\n", quote = FALSE)
  
  return(appendix_filename)
}

species_appendix(species_sheet)
