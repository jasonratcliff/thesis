# Appendix Script
species_appendix <- function(species_tab) {
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
  spec_app_subset <- spec_app[which(is.na(spec_app$App.A )), ]
  write.table(x = spec_app_subset,
              file = paste0("Appendix/", gsub(" +", "", species_tab), 
                            "_appendix.tsv"), 
              sep = "\t", eol = "\n", quote = FALSE)
}

species_appendix("P. acutifolia")
