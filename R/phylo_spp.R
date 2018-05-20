phylo_spp <- function(species) {
  
  # Subset the species to plot on the ggtree build
  species_geom <- ml_data_ext[grep(species,
                                   ml_data_ext$Physaria_a_priori_1), ]
  
  # Assign manual shapes for each state
  state_shapes <- c("Wyoming" = 0, "Montana" = 1,
                    "Colorado" = 2, "Idaho" = 3,
                    "New Mexico" = 4, "Utah" = 5,
                    "Some_where" = NULL, "NA" = NA)
  
  # Assign ggtree object from the expanded multi locus data set. 
  phylo_taxa <- ggtree(ml_data_ext) +
    
    # Plot prior IDs as the color aesthetic and State as shape.
    geom_point(data = species_geom,
               aes(colour = Physaria_a_priori_1,
                   shape = State), size = 3) + 
    
    # Set scale legend options for color, shape, and size.
    scale_colour_discrete("Physaria a priori") +  # label color legend by prior
    scale_shape_manual(values = state_shapes) +  # manual shape scale by State
    scale_size(labels = NULL) + 
    
    # Display text geom for each accession tip.
    geom_text(data = species_geom,
              aes(label = Physaria_a_priori_1), size = 3.1,
              hjust = -.2) + # , position = "right") +
    
    # Adjust position and direction of legend.
    theme(legend.position = c(0, 1), 
          legend.justification = c(0, 1), 
          legend.direction = "vertical") +  
    guides(colour = guide_legend(ncol = 1, byrow = TRUE)) +   # limit one column
    
    # Adjust x axis to accomodate text geoms.
    expand_limits(x = 0.035)
  
  # Prevent clipping of text geoms outside plot boundary.
  # From: https://gist.github.com/primaryobjects/700fe43b9631412fe0e1
  phylo_tree <- ggplot_gtable(ggplot_build(phylo_taxa))
  phylo_tree$layout$clip[phylo_tree$layout$name == "panel"] <- "off"
  
  # Return ggmap to output
  return(phylo_tree)
}