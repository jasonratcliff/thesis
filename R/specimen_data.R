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

# 2. Check date format ----

#' Write date mismatch log files
#' 
#' In the herbarium specimen .xlsx file, dates were converted to an 
#' Excel Date format "mm/dd/yyyy" (e.g. 06/15/2003) and saved to a new column
#' using the expression '=TEXT(<CELL>, "mm/dd/yyyy")'.  That column was copied
#' and saved to a new column using 'paste special...' by value.  The 
#' `date_mismatch()` function writes two log files to the subdirectory 
#' `log/date_logs/` by checking a regular expression against the string in the 
#' respective date columns of data frames returned by `read_specimens()`.
#' 
#' @param specimen_list List of data frames returned by `read_specimens()`
date_mismatch <- function(specimen_list) {

  # Create log directories and open file connections.
  dir.create("log", showWarnings = FALSE)
  dir.create("log/date_logs", showWarnings = FALSE)
  path_collection <- "log/date_logs/collection_dates.txt"
  path_identification <- "log/date_logs/identification_dates.txt"
  file_collection <- file(description = path_collection, open = "w")
  file_identification <- file(description = path_identification, open = "w")
  
  # Apply log file output function to list of file connections
  # for collection and identification date columns.
  invisible(lapply(list(file_collection, file_identification), 
    function(file_connection) {
      
      # Set file variables based on the file connection summary string.
      file_description <- summary(file_connection)$description
      if (grepl("collection", file_description))  { 
        file_col <- "Date" 
        date_regex <- "[0-1][0-9]/[0-3][0-9]/[1-2][0-9][0-9][0-9]"
        }
      if (grepl("identification", file_description)) { 
        file_col <- "ID" 
        date_regex <-"[!C\\?] [0-1][0-9]/[0-3][0-9]/[1][5-9]"
        }
      
      # For each specimen data frame, subset non-matching row indices and 
      # date strings and write table to file.
      for (i in seq_along(specimen_list)) {
        cat(names(specimen_list)[i], 
            paste(c("nrow:", "ncol:"), dim(specimen_list[[i]])), "\n",
            file = file_connection, append = TRUE, sep = "\n")
        date_fix <- as.matrix(grep(date_regex, 
                                   specimen_list[[i]][, file_col],
                                   invert = TRUE))
        date_fix <- cbind(date_fix, 
                          specimen_list[[i]][date_fix, file_col])
        write.table(date_fix, file = file_connection, append = TRUE, 
                    quote = FALSE, row.names = FALSE, 
                    col.names = FALSE, sep = "\t\t")
        cat("\n\n", file = file_connection, append = TRUE)
        }
      }))
  close(file_collection)
  close(file_identification)
}

date_mismatch(specimen_list)
