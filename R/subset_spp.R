subset_spp <- function(taxa_frame, state, county = NA, 
                       spp_str = NA, taxa_id = NA, exclude = FALSE,
                       longitude = NULL, latitude = NULL, 
                       set_name = NA, save_set = FALSE) {
  # Function to subset data frame by input state and county information.
  #   Includes optional regular expression to filter by species.
  #
  # Args:
  #   taxa_frame: Data frame from which to subset taxa of interest.
  #   state: Character vector of state for grep.
  #   county: Chacter vector of length >= 1 containing counties for grep.
  #   spp_str: Regular expression by which to subset specimen IDs.
  #   taxa_id: Character vector for the ID column by which to grep.
  #   exclude: Logical vector to invert grep call.
  #   set_name: Optional string to name the subset in directory "Phys_sets/".
  #   save_set: Logical vector where TRUE writes "Phys_sets/*.csv" from subset.
  #     - File output named by concatenation of STATE and COUNTY strings.
  #
  
  # Separate COUNTY character vector elements by "|" if length greater than 1.
  if (length(county) > 1) {
    county <- paste(county, collapse="|")
  }
  
  # Separate STATE character vector elements by "|" if length greater than 1.
  if (length(state) > 1) {
    state <- paste(state, collapse="|")
  }
  
  if (length(spp_str) > 1) {
    spp_str <- paste(spp_str, collapse="|")
  }
  
  if (is.na(county)) {
    taxa_subset <- taxa_frame[grep(state, taxa_frame$State), ]
  }
  
  if(!is.na(state) & !is.na(county)) {
    # Subset intersecting set of STATE and COUNTY rows into new data frame.
    taxa_subset <- taxa_frame[intersect(grep(state, taxa_frame$State),
                                        grep(county, taxa_frame$County)), ]
  } 
  
  # Filter optionally by SPECIES.
  if (!is.na(spp_str) & !is.na(taxa_id)) {  # regular expression representing taxa ID
    taxa_subset <- taxa_subset[grep(spp_str,  # return rows matching SPECIES
                                    taxa_subset[, taxa_id],
                                                invert = exclude), ]
  }
  
  ## Process COUNTY char vector for file naming to handle multiple counties.  
  # Replace OR RegEx "|" and spaces " " with "_" in COUNTY.
  county <- gsub("\\|| ", "_", county)
  # Split string by "_" into list and return list of substrings of length 3.
  county <- lapply(strsplit(county, "_"), substr, 1, 3)
  # Collapse character vector from list COUNTY by underscores.
  county <- paste(county[[1]], collapse = "_")
  # Join STATE substring (k=3) with collapsed COUNTY vector.
  frame_name <- paste(substr(state, 1, 3), county,
                      "Phys", sep = "_")  # append "Phys" separated by "_"
  
  # Subset the taxa data frame by latitude.
  if(!is.null(latitude)) {
    if(length(latitude) != 2) {  # Check length of latitude vector
      stop("Please enter min and max latitude values.")
    } else {
      taxa_subset <- subset(taxa_subset, 
                            taxa_subset$Latitude > latitude[1] & 
                              taxa_subset$Latitude < latitude[2])
    }
  }
  
  # Subset the taxa data frame by longitude.
  if(!is.null(longitude)) {
    if(length(longitude) != 2) {  # Check length of longitude vector
      stop("Please enter min and max longitude values.")
    } else {
      taxa_subset <- subset(taxa_subset, 
                            taxa_subset$Longitude > longitude[1] & 
                              taxa_subset$Longitude < longitude[2])
    }
  }
  
  # Option to save .csv from subset in subdirectory "Phys_sets/".
  if(save_set == TRUE) {
    # Check for presence of subdirectory to store function output.
    if(!dir.exists("Phys_sets/")) { # If absent,
      dir.create("Phys_sets/") # establish subdirectory.
    }
    
    # Write file named by either FRAME_NAME or SET_NAME.
    if(!is.na(set_name)) {
      # Write data to the project subdirectory "Phys_sets/" named by SET_NAME.
      write.csv(taxa_subset, 
                file=paste("Phys_sets/", set_name, ".csv", sep=""), 
                row.names=FALSE)
    } else {
      # Write data to the project subdirectory "Phys_sets/" named by FRAME_NAME.
      write.csv(taxa_subset,
                file=paste("Phys_sets/", frame_name, ".csv", sep=""), 
                row.names=FALSE)
    }
  }
  
  # Return subsetted taxa dataframe
  return(taxa_subset)
  
}
