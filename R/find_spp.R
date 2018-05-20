find_spp <- function(taxa_frame, priors = FALSE, locale = FALSE, geom = FALSE,
                     collection_number = numeric(), collector = character(), 
                     row_id = numeric()) {
  # Function to find specimen based on input collection number.
  #
  # Args:
  #   taxa_frame: Data frame from which to find specimen of interest.
  #   collection_number: Numeric vector of specimen identification number. 
  #   collector: Character vector representing botanist, preferably last name.
  #   row_id: Numeric vector to index row of input data frame.
  #   priors: Logical vector, includes prior identification columns when TRUE.
  #   locale: When TRUE, returns herbarium, state, and county of collection.
  #   geom: When TRUE, returns latitude and longitude coordinates.
  #
  # Establish indices to subset specimen information from taxa data frame.
  col_index <- c("Taxon_a_posteriori", "Collector", "Collection_Number")
  
  if (priors == TRUE) {  # option to include prior specimen identifications
  # Include all columns matching RegEx "Physaria_a_priori".
  col_index <- append(col_index, # Append names matching prior IDs RegEx.
                      names(taxa_frame)[grep(pattern = "Physaria",
                                        names(taxa_frame))])
  }
  
  if (locale == TRUE) {  # option to append locality data
    col_index <- append(col_index, c("Herbarium", "State", "County"))
  } # Append name elements for locality data to COL_INDEX.
  
  if (geom == TRUE) {  # option to append geographic coordinates
    col_index <- append(col_index, c("Latitude", "Longitude"))
  } # Append name elements for decimal degree data to COL_INDEX.
  
  # Check input values to verify set does not have to many arguments.
  if(length(collection_number) > 0 & length(collector) > 0 & 
     length(row_id) > 0) {
    print(c("Too many arguments.",
           "Pass collection_number with or without collector...",
           "OR pass only row_id."))
    stop()
  } # Stop function call for too many input variables.
  
  # If COLLECTOR is the only argument passed,
  # store all specimens matching COLLECTOR.
  if(length(collector) > 0 & length(collection_number) == 0 &
     length(row_id) == 0) {
    # Data frame indexed by rows of COL_INDEX.
    taxa_subset <- taxa_frame[grep(collector, 
                                   taxa_frame$Collector), col_index]
  } # Establish data frame by COLLECTOR.

  # If COLLECTION_NUMBER is the only argument passed, 
  # store specimens matching COLLECTION_NUMBER.
  if (length(collection_number) > 0  & length(collector) == 0 
      & length(row_id == 0) == 0) {
    # Data frame indexed by rows of COL_INDEX.
    taxa_subset <- taxa_frame[grep(pattern = collection_number, 
                                   taxa_frame[, "Collection_Number"]), 
                              col_index]
  } # Establish data frame by COLLECTION_NUMBER.
  
  # If COLLECTION_NUMBER and COLLECTOR have non-missing values,
  # store intersecting set from TAXA_FRAME matching input arguments.
  if (length(collection_number) > 0 & length(collector) > 0
      & length(row_id) == 0) {
    taxa_subset <- taxa_frame[intersect(grep(pattern = collection_number, 
                                             x = taxa_frame$Collection_Number),
                                        grep(pattern = collector,
                                             x = taxa_frame$Collector)),
                              col_index]
  } # Establish data frame by intersection of collection_number & collector.
  
  # Identify single row by numeric ROW_ID.
  if (length(row_id) > 0 & length(collection_number) == 0
      & length(collector) == 0) {  
    # Establish row of TAXA_FRAME by COL_INDEX.
    taxa_subset <- taxa_frame[row_id, col_index]
  } # Establish date frame by target row from TAXA_FRAME.
  
  return(taxa_subset)  # returns data frame TAXA_SUBSET
}
