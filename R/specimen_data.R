# 1. Read in specimen data ----

#' Read herbarium specimen data into R from an .xlsx file
#' 
#' An Excel file located in the `data/` subdirectory contains specimen voucher 
#' information for project herbarium records.  These data are used for 
#' analysis of distribution, morphology, ecology, and phylogenetics.  Records 
#' contain geographic coordinate data in degree decimal format, collection 
#' dates, annotation information, trait measurements and observations.
#' 
#' @param herbarium_file Path to .xlsx file with herbarium specimen records.
#' @return Large list of data frames of species tabs.
read_specimens <- function(herbarium_file) {
  
  # Vector of .xlsx spreadsheet tabs to read.
  species_tabs <- c("P. Remaining", "P. acutifolia", "P. saximontana",
                    "P. eburniflora", "P. didymocarpa", "P. chambersii",
                    "P. vitulifera_CO", "P. vitulifera_WY",
                    "P. integrifolia", "P. Idaho", "P. others",
                    "P. DNA", "P. NC_WY", "P. brassicoides")
  
  # Assign list of data frames read from excel tabs and filter by annotation.
  specimen_list <- lapply(species_tabs, function(xlsx_tab) {
    read.xlsx(xlsxFile = herbarium_file, sheet = xlsx_tab,
              cols = c(1:4, 5:8, 10:14, 15:17, 19:21, 28:65),
              colNames = TRUE, rowNames = FALSE, detectDates = TRUE) %>% 
      filter(grepl("Physaria|Lesquerella|Brassicaceae", Taxon))
  })
  names(specimen_list) <- species_tabs
  return(specimen_list)
}

specimen_list <- read_specimens("data/Phys_species_totals.xlsx")

