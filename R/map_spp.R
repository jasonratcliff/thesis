# Load packages
require(ggplot2)
require(ggmap)

map_spp <- function(map_frame, base_opt, taxa_id, 
                    mapped_spp = c("skip", "confirmed", 
                                   "questioned", "remain"),
                    map_x = "Longitude", map_y = "Latitude", f_adj = 0.075,
                    jitter_geoms = TRUE, geom_size = 3, jitter_pos = c(0, 0),
                    map_borders = "black", map_fill = NA, shape_opt = NULL,
                    id_spp = FALSE, save_plot = FALSE, map_name = NULL, 
                    collection_number = NULL, collector = NULL) {
  # Function to map specimens by subset data frame.
  #
  # Args:
  #   map_frame: Data frame (i.e. subset_spp() output) of specimens to map.
  #   base_opt: ggmap object base layer returned by topo_spp().
  #     Passing string "map_base" plots simple county & state border map.
  #   taxa_id: aesthetic mapping for color of map geoms.
  #     Use "Taxon_a_posteriori" | "Physaria_a_priori[_1..] | "Physaria_syn"
  #   map_x: Default value for aesthetic string for x axis (Longitude).
  #   map_y: Default value for aesthetic string for y axis (Latitude).
  
  # Build ggmap base layer from county and state boundary geom data.
  if (is.null(map_borders)) {
    stop(c("Enter character vector of length 1 for argument MAP_BORDERS", 
           "\n", 
           "Argument sets mapping asethetic 'color' for map border layer."))
  }
  
  ## Subset data to ensure coordinates are in degree decimal notation.
  #  Regular Expressions :
  # '[]' denotes a character class
  # '+' metacharacter matches the preceding expression one or more times
  # '\\' is used as an escape character to match '.' literally
  # '.' is itself a metacharacter that otherwise matches any character
  # '?' non- or single occurence of the preceding character
  # '^' denotes begining of string, while '$' denotes end
  #
  ## Subset rows by RegEx for $Latitude and $Longitude from input MAP_FRAME.
  phys_mapped <- subset(map_frame,  # Verify degree decimal notation
                        grepl("^-?[0-9]+\\.?[0-9]+?",  # for both 
                              map_frame$Longitude) &  # $Longitude &
                          grepl("[0-9]+\\.?[0-9]+", 
                                map_frame$Latitude) )  # $Latitude
  
  # Check options from MAPPED_SPP to grep rows from TAXA_ID data.
  { # Subset taxa needing confirmation of identification.
    if(mapped_spp == "questioned") {
      mapped_questioned <- phys_mapped[grep("\\?", phys_mapped[, taxa_id]), ]
      phys_mapped <- mapped_questioned
    }
    
    # Subset taxa with no reviewed identification.
    if(mapped_spp == "remain") {
      mapped_remain <- phys_mapped[is.na(phys_mapped[, taxa_id]), ]
      phys_mapped <- mapped_remain
    }
    
    # Subset taxa with non-questioned identifications - may include missing IDs.
    if(mapped_spp == "confirmed") {
      mapped_confirmed <- phys_mapped[grep("\\?", phys_mapped[, taxa_id], 
                                           invert = TRUE), ]
      phys_mapped <- mapped_confirmed
    }
  }
  
  ## The ggplot2 function 'map_data()' returns data frame from R package
  # "maps" (Brownrigg et al. 2018) with column names: 
  #  $long - NOTE: coordinates west of prime meridian are negative
  #  $lat
  #  $order - order in which to plot coordinates
  #  $region - state string
  #  $subregion - county string
  #  $group - structures how points are plotted
  
  # Build map base layer from state and county border "maps" data frames.
  { 
    mapped_env <- new.env() # Environment MAPPED_ENV for intermediate objects.  
  mapped_env$states <- c("montana", "wyoming", "idaho", "colorado", "utah", 
                         "washington","oregon", "nevada", "arizona", 
                         "new mexico", "nebraska", "north dakota", 
                         "south dakota", "california")  # char vect of states
    
  # Return data frames for state and county borders.
  mapped_env$mapped_states <- map_data("state", region = mapped_env$states)
  mapped_env$mapped_counties <- map_data("county", region = mapped_env$states)
  
  # Initialize ggplot base map layer of county lines with state borders.
  mapped_env$map_base <- ggplot(data = mapped_env$mapped_counties,
                                mapping = aes(x = long, y = lat, 
                                              group = group)) + 
    geom_polygon(color = map_borders, fill = map_fill, size = .125) +
    geom_polygon(data = mapped_env$mapped_states, fill = NA, 
                 color = map_borders, size = 1.5)  + 
    theme(panel.grid = element_blank(), panel.background = element_blank()) }
  
  # Option to plot basic map over state and county borders.
  if (base_opt == "map_base") {
    
    base <- mapped_env$map_base  # ggplot base layer of map_data
    map_plot <- base +  # add geoms by variable TAXA_ID.
      geom_jitter(data = phys_mapped, 
                 mapping = aes_string(x = map_x, y = map_y, 
                                      colour = taxa_id, shape = shape_opt),
                 size = geom_size, inherit.aes = FALSE,
                 width = jitter_pos[1], height = jitter_pos[2]) +
      
      # Adjust the axes using grDevices::extendrange() function
      coord_fixed(xlim = extendrange(phys_mapped$Longitude, f = f_adj),
                  ylim = extendrange(phys_mapped$Latitude, f = f_adj))
  }
  
  # Build map on ggmap base layer output by topo_spp() function.
  else {
    
    # Get ggmap object from Global Environment as the base layer.
    map_base <- get(base_opt, envir = .GlobalEnv)
    
    # Index the boundary of the ggmap plot to the x/y limits of ggmap BASE.
    # Attribute "bb" holds boundary border min/max lat and lon coordinates.
    map_xlim = c(attr(map_base, "bb")$ll.lon,
                 attr(map_base, "bb")$ur.lon)  # Longitude
    map_ylim = c(attr(map_base, "bb")$ll.lat,
                 attr(map_base, "bb")$ur.lat)  # Latitude
    
    # Plot satellite base layer with county border geom polygon layer
    map_plot <- ggmap(ggmap = map_base,  
                      # Call of base_layer substitutes default base layer.
                      # Overlays county boundaries on top of get_map() object.
                      base_layer = ggplot(data = mapped_env$mapped_counties,
                                          mapping = aes(x = long, y = lat),
                                          environment = mapped_env), 
                      extent = "normal", maprange = FALSE) +
      geom_polygon(aes(group = group),  # County geom aesthetics
                   color = map_borders, fill = map_fill, size = .25) + 
      geom_polygon(data = mapped_env$mapped_states,  # State geom aesthetics
                   aes(group = group), fill = NA,
                   color = map_borders, size = 1.5)  +
      
      # Set Mercator projection by boundary of ggmap lat and lon attributes.
      coord_map(projection = "mercator", xlim = map_xlim, ylim = map_ylim)
    
    # Geom jitter layer of TAXA_ID observations.
    if (jitter_geoms == TRUE) { 
      map_plot <- map_plot +
        geom_jitter(data = phys_mapped,
                    mapping = aes_string(x = map_x, y = map_y, 
                                         color = taxa_id, shape = shape_opt),
                    na.rm = TRUE, size = geom_size, 
                    width = jitter_pos[1], height = jitter_pos[2])
      }
  }
    
  # Optional argument to identify a single specimen geographically.
  # Executes readLine() for interactive input of COLLECTION_NUMBER & COLLECTOR.
  if (id_spp == TRUE) {
    
    if (!exists(ls(pattern = "find_spp",
                   envir = .GlobalEnv))) {
      source("R_scripts/R_functions/find_spp.R")  
    }
    
    if (is.null(collection_number) | is.null(collector)) {
      stop("Check Collection Number and Collector arguments...")
    }

    # Store single row data frame returned by unique specimen info.
    spp_id <- find_spp(taxa_frame = map_frame,
                       collection_number = as.numeric(collection_number),
                       collector = as.character(collector),
                       geom = TRUE, priors = TRUE)
      
    # Plot additional map layer to include the specimen returned by find_spp().
    map_plot <- map_plot +
      geom_point(data = spp_id, inherit.aes = FALSE,
                 mapping = aes(x = Longitude, y = Latitude), 
                 size = 5, shape = 23, colour = "black", fill = "gold") +
      geom_label(data = spp_id, inherit.aes = FALSE,
                 mapping = aes(x = Longitude, 
                               # Scale label height adjustment to lat range
                               y = Latitude + ((1/8) * 
                                 diff(range(map_plot$coordinates$limits$y))),
                               label = paste(collector,
                                             collection_number, sep = "\n")))
    }
  
  # Option to write ggplot to PDF saved by MAP_NAME.
  if (save_plot == TRUE) {
    if(is.null(map_name)) { 
      stop("Input character vector of length 1 for file output.")
    }
    if(!dir.exists("Phys_maps/")) {
      dir.create("Phys_maps/")
    }
    # Function ggplot2::ggsave() writes plot output as .pdf files.
    ggsave(filename=paste(as.character(map_name), "pdf", sep="."),
           device="pdf", path="Phys_maps")
    
  }
  # Return ggmap
  return(map_plot)
}
