library(openxlsx) # Reads .xlsx spreadsheet files.

### Column index names from .xlsx file.
# 1. Taxon                  # 6. Herbarium        # 12. ID           
# 2. Taxon_a_posteriori     # 7. State            # 15. Elev_.m.
# 3. Collector              # 8. County           # 16. Elev_.ft.         
# 4. Collection_Number      # 10. Latitude        # 17:21 - DNA / Fruit
# 5. Date                   # 11. Longitude       # 28:65 - Trait Data

# Establish an environment to hold data frames split from the .xlsx file.
taxa_env <- new.env()

# Establish character vector of species for data read into taxa environment.  
species_list <- c("P. Remaining", "P. acutifolia", "P. saximontana", 
                  "P. eburniflora", "P. didymocarpa", "P. chambersii",
                  "P. vitulifera_CO", "P. vitulifera_WY", 
                  "P. integrifolia", "P. Idaho", "P. dornii", "P. condensata",
                  "P. DNA", "P. NC_WY", "P. brassicoides")

# Assign species data frames matched by SPECIES_LIST to environment TAXA_ENV. 
load_species <- function(species_list) {
  
  # Element SPECIES of list SPECIES_LIST matchex an individual .xslx tab name.
  for (species in species_list) {
    
    # Extract .xlsx tab for each string SPECIES in list SPECIES_LIST.
    species_data <- openxlsx::read.xlsx("Phys_species_totals.xlsx",
                                        # List element SPECIES.
                                        sheet = species,
                                        cols = c(1:4, 5:8, 10:12,
                                                 15:17, 19:21, 28:65),
                                        colNames = TRUE, rowNames = FALSE, 
                                        detectDates = TRUE)

    # Subset data so $Taxon contains the substring "Physaria".
    species_cleaned <- species_data[grep("Physaria|Lesquerella", 
                                         species_data$Taxon, invert = FALSE),]

    # Assign data frame to TAXA_ENV by SPECIES character string with removed
    # period & whitespace from Excel tab name using function gsub().
    assign(gsub(". ", "_", species), 
           species_cleaned, envir = taxa_env) 
    }
} 

load_species(species_list) 
rm(load_species)

# Store list of strings matching individual species data frames.  
taxa_env$species_df_names <- ls(pattern = "P_*", envir = taxa_env)

# Use mget() to establish a large list of data frame elements,
# each corresponding to an Excel tab name parsed by load_species().
taxa_env$species_data <- mget(taxa_env$species_df_names, 
                              envir = taxa_env)

# Once combined, removed individual species data frames from TAXA_ENV.
rm(list = ls(pattern = "^P_", envir = taxa_env), envir = taxa_env)

# Generate log file containing date mismatch output for
# collection and identification dates (i.e. $Date, $ID columns).
# Files are written to "Phys_logs/" subdirectory.
source("R/date_mismatch.R") 
date_mismatch()
rm(date_mismatch)

# Iterate data frames in list TAXA_ENV$SPECIES_DATA to extract character vector
# $Taxon of prior identifications from each data frame SPECIES.  
# Identifications for each specimen are stored as a character vector element of 
# list TAXA_A_PRIORI.  Prior identifications separated by a comma (",")
# are split into a character vector with multiple elements.
# 
get_priors <- function() {

  i <- 1  # Initialize for loop counter.
  
  for (species in taxa_env$species_data) {
    
    # Extract $Taxon from SPECIES_DATA to index additional tables from.
    taxa_priors <- species$Taxon
    
    # Initialize empty list for a priori identifications.
    taxa_a_priori <- list()
    
    j <- 1 # Initialize nested for loop counter.
    
    # Nested loop through of $Taxon splits prior specimen IDs by comma.
    for (taxon in taxa_priors) {
      if (grepl(",", taxon))  {
        # Concatenate strings split by commas to TAXA_A_PRIORI.
        taxa_a_priori[j] <- strsplit(taxon, ", ")
        # Rows with a single ID are stored as is. 
      } else { 
        taxa_a_priori[j] <- taxon 
        }
      j <- j + 1
    }
    
    # Assign list named by SPECIES with substring "taxa_a_priori" prepended. 
    assign(paste("a_priori", names(taxa_env$species_data)[i], sep="_"), 
           taxa_a_priori, envir = taxa_env) 
    
    i <- i + 1 # Plus one for the counter.  
    
  } 
} # Assigns a priori lists to environment TAXA_ENV.

get_priors()
rm(get_priors)


prior_split <- function() {
  
  # Initialize numeric vector to hold counts of max A_PRIORI_P* lengths 
  # (i.e. max number of IDs per single specimen).  
  taxa_count <- numeric()
  
  # Iterate through all lists matching "a_priori" in TAXA_ENV.  
  # Use sapply() of function length() to taxa ID list elements and 
  # save maximum into array TAXA_COUNT to return the maximum number of IDs
  # for each species list of a priori IDs.
  for (taxa_id in ls(taxa_env, pattern = "a_priori", 
                    envir = taxa_env)) {
    taxa_count <- c(taxa_count, 
                    max(sapply(get(taxa_id, envir = taxa_env), 
                               length)))
  } 
  
  rm(taxa_id)
  taxa_count <- max(taxa_count)  # determine max per specimen ID number.

  # Iterate through list of a priori IDs and split list elements 
  # into data frames via PLYR package.  Each species data frame is 
  # written to a respective .csv file in the "Phys_tidy/" subdirectory.
  for (taxa_id_a_priori in ls(pattern = "a_priori*", 
                              envir = taxa_env)) {
    
    # Store substring representing taxa for indexing to full data frame.
    taxon <- strsplit(taxa_id_a_priori, 
                      split = "a_priori_")[[1]][2]
    
    # PLYR package :: convert a priori identification list to data frame, 
    # binding list elements by row.
    taxa_id_list <- get(taxa_id_a_priori, envir = taxa_env)
    species_key <- plyr::ldply(taxa_id_list, rbind)
    
    # Add empty columns to equal length of max int in TAXA_COUNT.  
    if (ncol(species_key) < taxa_count) {
      
      # Calculate difference between max and current ncol.
      for (i in 1:(taxa_count - ncol(species_key))) {
        # Bind respective number of columns filled by NA values.
        species_key <- cbind(species_key, NA)
      }
    }
    
    # Rename columns by appending "Physaria_a_priori" to each column number.
    for (column in 1:ncol(species_key)) { 
      colnames(species_key)[column] <- paste("Physaria_a_priori", 
                                             column, sep = "_")
    }
    
    # Bind column for most recent a priori identification.
    species_key <- cbind(species_key, NA)
    names(species_key)[ncol(species_key)] <- "Physaria_recent"
    # Replace factor levels as character vectors.
    for (i in 1:ncol(species_key)) {
      species_key[, i] <- as.character(species_key[, i])
    }
    
    # Iterate through data frame rows to store most recent identification 
    # in SPECIES_KEY column $Physaria_recent.
    for (i in 1:nrow(species_key)) {
      
      # Nested loop through data frame columns.
      for (j in 1:ncol(species_key)) { #[i, ])) {
        
        # If value is not missing, check for identity confirmation "!".
        if (!is.na(species_key[i, j])) {
          if (species_key[i, j] == "!") {
            # Replace character "!" with previous identification.
            species_key[i, j] <- species_key[i, j-1] 
          } 
        }
        # If value is missing, store most recent non-missing value
        # in column $Physaria_recent.
        if (is.na(species_key[i, j])) {
          species_key$Physaria_recent[i] <- species_key[i, j-1]
          break # Break loop, return to next row for iteration.
        }
      }
    }
    
    # Bind column for identifications substituted by synonym.
    species_key <- cbind(species_key, NA)
    names(species_key)[ncol(species_key)] <- "Physaria_syn"
    
    # Bind species data frame columns to parsed SPECIES_KEY.
    species_key <- cbind(species_key, 
                         taxa_env$species_data[[grep(taxon,
                                                names(taxa_env$species_data))]])
                        
    # Assign 'tidy' data frame with split a priori ID lists to TAXA_ENV.
    assign(gsub("a_priori_", "tidy_", taxa_id_a_priori),
           species_key, envir = taxa_env)
  }
  
}

prior_split()
rm(prior_split)

taxa_frame <- function() {
  
  # Establish subdirectory to hold tidy data frames for individual species.
  if (!dir.exists("Phys_tidy/")) {
    dir.create("Phys_tidy/")
  } else {
    # Remove existing directory and recursively delete files within.
    if (dir.exists("Phys_tidy/")) {
      unlink("Phys_tidy/", recursive = TRUE)
      dir.create("Phys_tidy/")
    }
  }
  
  # Write .csv files for each species data frame matching "tidy_" in TAXA_ENV.
  for (tidy in ls(pattern = "tidy_", 
                  envir = taxa_env)) {
    tidy_df <- get(tidy, envir = taxa_env)  # get parsed species data frame
    write.csv(tidy_df, 
              file=paste("Phys_tidy/", tidy, ".csv", sep=""), 
              row.names=FALSE)
  }
  
  # Row bind tidy data frames from "Phys_tidy/" subdirectory.
  total_physaria <- do.call(rbind, 
                            lapply(list.files("Phys_tidy/", 
                                              full.names=TRUE), 
                                   read.csv, stringsAsFactors=FALSE))
  
  # Remove concatenated data frame row names and assign to global environment.
  row.names(total_physaria) <- NULL
  assign("total_physaria", total_physaria, envir = .GlobalEnv) 

}

taxa_frame()
rm(taxa_frame, taxa_env, species_list)


taxa_syn <- function() {
  
  # Create log subdirectory for synonyms files.
  if(!dir.exists("Phys_logs/synonyms")) {
    dir.create("Phys_logs/synonyms")
  }
  
  # Physaria acutifolia synonyms
  acut_syn <- c("^Physaria acutifolia var. purpurea$",
                "^Physaria acutifolia var. stylosa$",
                "^Physaria australis$",
                "^Physaria didymocarpa var australis$",
                "^Physaria didymocarpa var. australis$",
                "^Physaria australis (Payson) Rollins$")
  
  # Physaria didymocarpa ssp didymocarpa synonyms
  didy_syn <- c("^Physaria didymocarpa (Hook.) A. Gray$",
                "^Physaria didymocarpa (Hook.) Gray$",
                "^Physaria didymocarpa (Hook.) Gray var.$",
                "^Physaria didymocarpa (Hook.) Gray var. didymocarpa$",
                "^Physaria didymocarpa var normalis$",
                "^Physaria didymocarpa var didymocarpa$",
                "^Physaria didymocarpa var. didymocarpa$",
                "^Physaria didymocarpa ssp didymocarpa$",
                "^Physaria didymocarpa$")
  
  # Physaria didymocarpa ssp lyrata synonyms
  lyra_syn <- c("^Physaria didymocarpa (Hook.) Gray var. lyrata$",
                "^Physaria didymocarpa (Hook.) Gray var. lyrata Hitch.$",
                "^Physaria didymocarpa ssp lyrata$")
  
  # Physaria didymocarpa ssp lanata synonyms
  lana_syn <- c("^Physaria didymocarpa ssp lanata$",
                "^Physaria didymocarpa var lanata$",
                "^Physaria lanata$")
  
  # Physaria integrifolia synonyms
  inte_syn <- c("^Physaria didymocarpa (Hook.) Gray var. integrifolia Rollins$",
                "^Physaria didymocarpa var. integrifolia$",
                "^Physaria integrifolia var integrifolia$",
                "^Physaria integrifolia var. integrifolia$",
                "^Physaria integrifolia var. monticola$")
  
  # Physaria saximontana ssp saximontana synonyms
  saxi_saxi_syn <- c("^Physaria saximontana$",
                     "^Physaria saximontana ssp saximontana$")
  
  # Physaria saximontana ssp dentata synonyms
  saxi_dent_syn <- "^Physaria saximontana ssp dentata$"
  
  # Establish list holding regular expression character vectors of 
  # specimen identification synonyms from $Physaria_recent.
  phys_syns <- list("acut" = acut_syn, "didy" = didy_syn, 
                    "lyra" = lyra_syn, "lana" = lana_syn,
                    "inte" = inte_syn, "saxi" = saxi_saxi_syn, 
                    "dent" = saxi_dent_syn)
  
  # Establish character vector holding synonym conversions to replace 
  # recently annotated specimens with names as described by O'Kane 2010.
  # Character vector names are shared by list PHYS_SYNS names.
  phys_conv <- c("acut" = "Physaria acutifolia",
                 "didy" = "Physaria didymocarpa ssp. didymocarpa",
                 "lyra" = "Physaria didymocarpa ssp. lyrata", 
                 "lana" = "Physaria didymocarpa ssp. lanata",
                 "inte" = "Physaria integrifolia", 
                 "saxi" = "Physaria saximontana ssp. saximontana", 
                 "dent" = "Physaria saximontana ssp. dentata")
  
  # Iterate through recent taxa IDs to replace synonyms.
  i <- 1  # Initialize counter for specimen annotation.
  for (taxa_id in total_physaria$Physaria_recent) {
    
    # Search for TAXA_ID in list of synonym regular expressions PHYS_SYNS.  
    # If TAXA_ID matches a PHYS_SYNS element, store numeric pointing to 
    # respective synonym element and ensure length is not greater than one.
    taxa_regex <- paste("^", taxa_id, "$", sep = "")
    
    if (TRUE %in% grepl(taxa_regex, phys_syns, fixed = TRUE)) {
      syn_ind <- names(phys_syns)[which(grepl(taxa_regex,
                                              phys_syns, 
                                              fixed = TRUE) == TRUE)]
      
      if(length(syn_ind) > 1) {
        stop("Multiple synonym indices detected. Check row", print(i))
        } else {
          
          # Save synonym regular expression to match replacement and
          # match name from PHYS_SYNS to name in PHYS_CONV to insert
          # synonym conversion in TOTAL_PHYSARIA column $Physaria_syn.
          phys_syn <- phys_syns[syn_ind]
          total_physaria$Physaria_syn[i] <- phys_conv[[names(phys_syn)]]
        }
      } else {
      
      # For rows not matching a synonym regular expression in PHYS_SYNS,
      # insert non-converted specimen annotation from $Physaria_recent
      # to column $Physaria_syn in TOTAL_PHYSARIA.
      total_physaria$Physaria_syn[i] <- total_physaria$Physaria_recent[i]
      
      }
    
    i <- i + 1 # counter plus one
    
  }

  # Write summary of most recent specimen annotations to log file.
  phys_syn <- file("Phys_logs/synonyms/priors_log.txt", open = "w")
  cat("Physaria Synonyms", 
      capture.output(as.matrix(table(as.factor(
        total_physaria$Physaria_recent)))),
      file = phys_syn, sep = "\n", append = FALSE)
  close(phys_syn)
  
  # Write summary of species ID synonym changes to log file.
  phys_conv <- file("Phys_logs/synonyms/post_log.txt", open = "w")
  cat("Physaria Synonyms Converted", 
      capture.output(as.matrix(table(as.factor(
        total_physaria$Physaria_syn)))),
      file = phys_conv, sep = "\n", append = FALSE)
  close(phys_conv)
  
  assign("total_physaria", total_physaria, envir = .GlobalEnv)
  
}

taxa_syn()
rm(taxa_syn)
