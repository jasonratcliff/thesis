### Date Mismatch ID
#
# Dates were first converted to an Excel Date format internally as mm/dd/yyyy 
# (e.g. 6/15/2003, 8/2/1859).  
# The value of those cells was saved to a new column by the expression, 
# '=TEXT(<CELL>, "mm/dd/yyyy")'.  
# That column was then copied and saved to a new column, 
# using 'paste special...' by value.  
# Date column strings are read into R by the 'openxlsx::read.xlsx()',
# from function call of load_species().  
#
date_mismatch <- function() {
  
  # Verify subdirectory to store log file for date_mismatch.R source.
  if (!dir.exists("Phys_logs/date_logs/")) { 
    
    # Verify directory structure for date log files.
    if (!dir.exists("Phys_logs/")) {
      dir.create("Phys_logs/")
      dir.create("Phys_logs/date_logs")  
    } else {
      dir.create("Phys_logs/date_logs")
    }
  }
  
  ### Species Collection Dates
  
  # Open file object to write log info for collection date mismatches.  
  collection_date <- file(description = paste("Phys_logs",
                                              "date_logs",
                                              "collection_dates.txt",
                                              sep = "/"), open = "w")
  
  cat("Specimen Collection Dates\n",
      file = collection_date, append = FALSE, sep = "\n")
  
  # Open file object to write log info for identification date mismatches.  
  identification_date <- file(description = paste("Phys_logs",
                                                  "date_logs",
                                                  "identification_dates.txt",
                                                  sep = "/"), open = "w")
  cat("Specimen Identification Dates\n",
      file = identification_date, append = FALSE, sep = "\n")
  
  i <- 1 # Initialize counter for iterating list of species data frame names.
  
  for (species in taxa_env$species_data) {
    
    # Concatenate name of SPECIES_DATA element to file COLLECTION_DATE.
    cat(names(taxa_env$species_data)[i], file = collection_date, 
        append = TRUE, sep = "\n")
    
    # Concatenate dimensions of the data frame.
    cat(paste(c("nrow:", "ncol:"), dim(species)), 
        file = collection_date, append = TRUE, sep = "\n")
    cat("\n", file = collection_date, append = TRUE)
    
    # Subset matrix indices matching inverse (i.e. non-matching) 
    # collection dates into first column of COLLECTION_DATES matrix.
    coll_dates <- as.matrix(grep(paste("[0-1][0-9]",
                                       "[0-3][0-9]",
                                       "[1-2][0-9][0-9][0-9]",
                                       sep = "/"),
                                       species$Date, invert = TRUE))
    
    # Bind row indices and non-matched specimen collection dates by column.
    coll_dates <- cbind(coll_dates, species$Date[coll_dates])
    
    # Concatenate COLL_DATES to file object COLLECTION_DATE separated by tabs.
    write.table(coll_dates, file = collection_date, 
                append = TRUE, quote = FALSE,
                row.names = FALSE, col.names = FALSE, sep = "\t\t")
    cat("\n\n", file = collection_date, append = TRUE)
  
      
    ### Species identification dates
    
    # Concatenate name of SPECIES_DATA element to file IDENTIFICATION_DATE.
    cat(names(taxa_env$species_data)[i], 
        file = identification_date, append = TRUE, sep = "\n")
    
    # Concatenate dimensions of the data frame.
    cat(paste(c("nrow:", "ncol:"), dim(species)), 
        file = identification_date, append = TRUE, sep = "\n")
    cat("\n", file = identification_date, append = TRUE)
    
    # Subset matrix indices matching inverse (i.e. non-matching) 
    # ID dates into first column of ID_DATES matrix.
    id_dates <- as.matrix(grep(paste("[!C\\?] ",  # Status
                                     "[0-1][0-9]/",  # Month
                                     "[0-3][0-9]/",  # Day
                                     "[1][5-7]",  # Year
                                     sep = ""),
                                 species$ID, invert = TRUE))
    
    # Bind row indices and non-matched specimen identification dates by column.
    id_dates <- cbind(id_dates, species$ID[id_dates])
    
    # Concatenate ID_DATES to file object IDENTIFICATION_DATE separated by tabs.
    write.table(id_dates, file = identification_date, 
                append = TRUE, quote = FALSE,
                row.names = FALSE, col.names = FALSE, sep = "\t\t")
    cat("\n\n", file = identification_date, append = TRUE)
  
    i <- i + 1
  }
  # Close file connections.
  close(collection_date)
  close(identification_date)
}
