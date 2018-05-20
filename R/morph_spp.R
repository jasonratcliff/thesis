morph_spp <- function(species_frame, species_path, species_name,
                      cont = FALSE, disc = FALSE) {
  # Function to subset data frames for discrete and continuous characters
  #
  # Args:
  #   species_frame: Data frame of specimens to analyze for morphology.
  #   species_path: Path to create "Phys_traits/" subdirectory for output.
  #   species_name: Character string representing species for discrete summary.
  #   cont: Logical vector, when TRUE analyze continuous traits.
  #   disc: Logical vector, when TRUE analyze discrete traits.
  
  # Establish character vectors to index discrete or continuous traits.
  
  continuous_traits <- c("Stem_length_dm", "Basal_leaf_length_cm",
                         "Cauline_leaf_length_mm", "Sepal_length_mm",
                         "Petal_length_mm", "Style_length_mm", 
                         "Mature_fruit_length_mm", "Mature_fruit_width_mm",
                         "Ovule_number", "Mature_seed_length_mm")
  
  discrete_traits <- c("Rosulate", "Caudex", "Pubescence", 
                       "Basal_leaf_trichomes", "Fruit_trichomes", "Stem_count", 
                       "Stem_shape", "Petiole", "Basal_leaf_shape", 
                       "Basal_leaf_margins", "Cauline_leaf_shape",
                       "Cauline_leaf_margins", "Racemes", "Pedicel_shape", 
                       "Pedicels_secund", "Sepal_shape", "Petal_color", 
                       "Petal_shape", "Fruit", "Mature_fruit_apices", 
                       "Replum_pubescence", "Inner_valve_pubescence", 
                       "Replum_shape", "Seed_color", "Seed_shape")
  
  # Check for existance of subdirectory "Phys_traits" in project directory.
  if(!dir.exists("~/Thesis/Physaria_spatial_analysis/Phys_traits")) {
    # Establish subdirectory to hold trait data split into .csv files.
    dir.create("~/Thesis/Physaria_spatial_analysis/Phys_traits")
  }
  
  ## Discrete data ##
  if(disc == TRUE) {
    
    # Check existence of species trait subdirectory.
    if(!dir.exists(paste("Phys_traits/", species_path, 
                         "/discrete", sep = ""))) {
      # Create subdirectories to store discrete data .csv files.
      dir.create(paste("Phys_traits/", species_path, 
                       "/discrete", sep = ""), recursive = TRUE)
    }
    
    # Subset the discrete trait data from input SPECIES_FRAME.
    disc_data <- species_frame[, names(species_frame) %in% discrete_traits]
    
    for (i in 1:ncol(disc_data)) {
      write.table(disc_data[, i], 
                  file =  paste("Phys_traits/", species_path, "/discrete/",  
                                names(disc_data[i]), 
                                ".csv", sep = ""), 
                  quote = FALSE, col.names = FALSE, row.names = FALSE)
    }
    
    # Write a new file containing the output of the morph.sh script
    system(paste("./R_scripts/morph.sh Phys_traits/", species_path,
                 "/discrete/", " > ", "Phys_traits/", species_path, "/", 
                "discrete_summary_", species_name, ".txt", sep=""))
  }
  
  ## Continuous data ##
  if(cont == TRUE) {
    
    # Check existence of species trait subdirectory.
    if(!dir.exists(paste("Phys_traits/", species_path, 
                         "/continuous", sep = ""))) {
      # Create subdirectories to store continuous data .csv files.
      dir.create(paste("Phys_traits/", species_path, 
                       "/continuous", sep = ""), recursive = TRUE)
    }
    
    # Subset the discrete trait data from input SPECIES_FRAME.
    cont_data <- species_frame[, names(species_frame) %in% continuous_traits]
    
    # Loop through length of columns, writing .csv files by continuous trait.
    for (i in 1:ncol(cont_data)) {
      write.table(cont_data[, i], 
                  file =  paste("Phys_traits/", species_path, "/continuous/",  
                                names(cont_data[i]), 
                                ".csv", sep = ""), 
                  quote = FALSE, col.names = FALSE, row.names = FALSE)
    }
    
  }
  
}
