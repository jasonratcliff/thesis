# Load dependency package "plyr"
# Hadley Wickham (2011). The Split-Apply-Combine Strategy for Data
# Analysis. Journal of Statistical Software, 40(1), 1-29. URL
# http://www.jstatsoft.org/v40/i01/.
library(plyr)

con_trait <- function(trait_frame, trait_name) {
  
  # Measure variable length of columns matching expression "Physaria_a_priori*"
  col_num <- length(grep("Physaria", names(trait_frame)))
  col_num <- col_num + 7  # addtional columns for specimen metadata
  
  # Remove rows with collection date not matching regular expression
  trait_dated <- trait_frame[grep(paste("[0-1][0-9]", "[0-3][0-9]", 
                                        "[1-2][0-9][0-9][0-9]", sep = "/"), 
                                  trait_frame$Date), ]

  # Establish empty data frame with 7 columns for cleaned trait data.
  trait_matrix <- as.data.frame(matrix(data = NA, ncol = col_num))
  
  # Provide data frame names for minimum, max, and mean trait measurement;
  # colection date; posterior ID; and collector / collection number.
  names(trait_matrix) <- c("Trait_Min", "Trait_Max", "Trait_Mean",
                           "Collection_Date", "Taxon_a_posteriori",
                           "Collection_Number", "Collector", 
                           # Character vector matchting a priori name columns.
                           names(trait_frame)[grep("Physaria", 
                                                   names(trait_frame))])
  
  # Establish a data frame to subset individual rows for data cleaning.
  trait_row <- as.data.frame(matrix(data = NA, ncol = col_num))
  
  # Use names consistent with TRAIT_MATRIX for row binding.
  names(trait_row) <- names(trait_matrix)
  
  # When TRAIT_NAME matches "Elev", parse elevation data.
  if (grepl("Elev", trait_name)) {
    
    # Remove "," from elevation data column of TRAIT_DATED.
    # Returns list of elevation data as ELEV_OBS.
    elev_obs <- lapply(trait_dated[, trait_name],
                       function(elevation) {
                         gsub(",", "", elevation)})
    
    # Substitute "s.n." for NA missing values from ELEV_OBS.
    elev_obs <- lapply(elev_obs,
                       function(elevation) {
                         gsub("s.?n.? ?", NA, elevation)})
    
    # Replace original elevation data with parsed elevation observations.
    # Use plyr::ldply to return a data frame with 1 col from a list.
    trait_dated[, trait_name] <- ldply(elev_obs)
    
  }
  
  # Iterate through number of TRAIT_DATED rows to process data.
  for (i in 1:nrow(trait_dated)) {
    
    # Check for missing data, and ignore row if missing.
    if (!is.na(trait_dated[i, trait_name])) {
      
      trait_val <- trait_dated[i, trait_name]
      trait_val <- gsub(",", "", trait_val)

      # If TRUE that row i of TRAIT_NAME matches "*-*" (i.e. contains dash),
      # split the row by the dash into a list of 2 elements.
      if (grepl("-", trait_val)) {
        
        # Split character string of range range data by "-".
        # Stores min and max values as list RANGE_LIST.
        range_list <- strsplit(trait_val, "-")
        
        # Store range and midpoint of trait value observations.
        trait_row$Trait_Min <- as.numeric(range_list[[1]][1])
        trait_row$Trait_Max <- as.numeric(range_list[[1]][2])
        trait_row$Trait_Mean <- as.numeric( { (trait_row$Trait_Min +
                                                trait_row$Trait_Max) / 2 } )
        
      } else { 
        
        # Store observations of a single value in all three columns.
        trait_row$Trait_Min <- as.numeric(trait_val)
        trait_row$Trait_Max <- as.numeric(trait_val)
        trait_row$Trait_Mean <- as.numeric(trait_val)
        
      }

      # Subset specimen metadata.
      trait_row$Collection_Date <- trait_dated$Date[i]
      trait_row$Taxon_a_posteriori <- trait_dated$Taxon_a_posteriori[i]
      trait_row[, grep("Physaria", names(trait_row))] <- 
        trait_dated[i, grep("Physaria", names(trait_dated))]
      trait_row$Collection_Number <- trait_dated$Collection_Number[i]
      trait_row$Collector <- trait_dated$Collector[i]
      
      # Bind TRAIT_ROW by row to TRAIT_MATRIX.
      trait_matrix <- rbind(trait_row, trait_matrix)
    }
  }
  
  # Format the dates as dd-mm character vectors.
  # Effectively removes the year from the date.
  trait_matrix$Collection_Date <- format(as.Date(trait_matrix$Collection_Date,
                                                format = "%m/%d/%Y"),
                                        format = "%d-%m")
    
  # Format the collection dates as "day - month" date class.
  trait_matrix$Collection_Date <- as.Date(trait_matrix$Collection_Date,
                                         format = "%d-%m")

  return(trait_matrix)

}
