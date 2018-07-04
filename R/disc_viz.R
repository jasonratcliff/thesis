library(plyr)

# Function to replace the first character of string elements in a 
# character vector with an uppercase letter of the first index. 
upper_fix <- function(trait_string) {
  sapply(trait_string, USE.NAMES = FALSE, 
         function(trait_substring) {
           if (!is.na(trait_substring)) {
             trait_substr <- substring(trait_substring, 
                                       2, nchar(trait_substring))
             capital <- toupper(substring(trait_substring, 1, 1))
             paste0(capital, trait_substr)
           }
         })
}

map_base <- function(map_borders = "black", map_fill = "ghostwhite") {
  states <- map_data("state")
  counties <- map_data("county")
  map_plot <- ggplot(data = counties,
                     mapping = aes(x = long, y = lat, 
                                   group = group)) + 
    geom_polygon(color = map_borders, fill = map_fill, size = .125) +
    geom_polygon(data = states, fill = NA, 
                 color = map_borders, size = 1.5)  + 
    theme(panel.grid = element_blank(), panel.background = element_blank())
  return(map_plot)
}

# Function to parse discrete trait observation data.
# Returns ggplot object of geom_point layers to build trait distribution models.
disc_viz <- function(specimens, trait, map_base = "map_base",
                     brewer_palette = "Spectral", brewer_type = "div") {
  
  # Subset specimen data by character vector for trait column as list.
  disc_trait_list <- as.list(specimens[, trait])
  
  # gsub brackets (less often observed traits), hyphens, and split by comma.
  disc_trait <- lapply(disc_trait_list, function(traits) {
    trait1 <- gsub(x = traits, pattern = "\\(", replacement = "")
    trait2 <- gsub(x = trait1, pattern = "\\)", replacement = "")
    trait3 <- gsub(x = trait2, pattern = "-", ", ")  # intergradation dash
    trait4 <- strsplit(x = trait3, split = ", ")  # split string by comma
    
    trait_obs <- lapply(trait4, upper_fix)  # replace lowercase letters
    traits <- unique(trait_obs)  # filter for unique trait values
    traits_list <- unlist(traits)  # unlist nested list
    traits_list
  })
  
  # Table index of unique trait character observations.
  disc_trait_table <- sort(table(unlist(disc_trait)), decreasing = TRUE)
  disc_trait_opts <- names(disc_trait_table)  # character vector of trait names
  col_index <- length(disc_trait_opts)  # column index for empty vector
  
  # Return list of traits split by observation and sorted as column index
  # by frequency of discrete trait observations.
  trait_list <- lapply(disc_trait, function(x) {
    trait_index <- which(disc_trait_opts %in% x)   # index trait observations
    trait_logic <- rep(FALSE, col_index)  # initialize empty logical vector
    trait_logic[trait_index] <- TRUE  # record indexed specimen observations
    trait_matrix <- matrix(NA, ncol = col_index)  # empty trait matrix.
    trait_matrix[trait_index] <- disc_trait_opts[trait_index]
    trait_matrix
  })
  
  # Convert trait list to data frame and store sequence of trait column names.
  trait_frame <- ldply(trait_list)
  names(trait_frame) <- paste(trait, seq_along(disc_trait_opts), sep = "_")
  
  # Subset data frame for specimen identifications, coordinates, and trait obs.
  specimen_traits <- specimens[, c(trait, grep("Physaria", 
                                               names(specimens), value = TRUE), 
                                   "Taxon_a_posteriori", "Collector", 
                                   "Collection_Number", "Longitude", "Latitude")]
  
  # Bind split trait data to specimen data.
  specimen_traits <- cbind(specimen_traits, trait_frame)
  
  # Select map base layer type for ggplot base layer.
  if (map_base == "map_base") {
    # Build basic state and county boundary map.
    map_base_plot <- map_base(map_fill = NULL)  
  }
  
  # Build ggplot layers of geom points for each discrete trait observation.
  plot_layers <- function(trait_df, trait_str, map_plot) {
    trait_names <- grep(paste0(trait_str, "_"), names(trait_df), value = TRUE)
    geom_sizes <- seq(10, 0.75, length.out = length(trait_names))
    j <- 1
    trait_plot <- map_plot
    for (name in trait_names) {
      trait_plot <- trait_plot +
        geom_point(aes_string(colour = name, shape = "Taxon_a_posteriori",
                              x = "Longitude", y = "Latitude"), inherit.aes = FALSE,
                   size = geom_sizes[j], na.rm = TRUE,
                   data = trait_df[which(!is.na(trait_df[, name])), ])
      j <- j + 1
    }
    trait_plot <- trait_plot + 
      coord_fixed(xlim = extendrange(specimens$Longitude, f = 0.075),
                  ylim = extendrange(specimens$Latitude, f = 0.075))
    return(trait_plot)
  }
  
  # Call function and add theme and guide parameters.
  discrete_plot <- plot_layers(trait_df = specimen_traits, 
                               trait_str = trait,
                               map_plot = map_base_plot) + 
    theme(legend.box = "vertical", legend.position = "right",
          legend.direction = "vertical", legend.title.align = 0.5,
          panel.background = element_blank()) +
    guides(colour = guide_legend(ncol = 2), shape = guide_legend(ncol = 1)) + 
    
    # Use colour brewer for discrete scale, named by parsed trait string. 
    scale_colour_brewer(sapply(trait, USE.NAMES = FALSE, function(x) {
      s1 <- gsub("[0-9]", "", x)
      s2 <- gsub("_", " ", s1)
      s3 <- unlist(strsplit(s2, " "))
      s4 <- upper_fix(s3)
      s5 <- paste(s4, collapse = " ")
    }), type = brewer_type, palette = brewer_palette)
  
  return(discrete_plot)
}
