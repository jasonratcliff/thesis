phylo_text <- function(dna_df, dna_label, 
                       long, lat, color_opt = "black") {
  # Function to return ggplot text geom layer for DNA specimen map plots.
  #
  # Args:
  #   dna_df: Data frame subset of <ml_data_ext> with column $label.
  #     - Should include mapped DNA specimens matching phylogenetic data.
  #   dna_label: Character vector matching specimen labels to annotate map.
  #     - Multiple labels are concatenated by "|" for grep call.
  #   long: Character vector of length 1 matching label longitudinal position.
  #   lat: Character vector of length 1 matching label longitudinal position.
  #     - Position correction can be applied to either long or lat by
  #     - by including "+" or "-" coordinate distance in character string.
  #   color_opt: Character vector of length 1 specifying label color.
  #
  phylo_geom <- geom_label(data = dna_df[grep(dna_label, 
                                              dna_df$label), ], 
                          inherit.aes = FALSE,
                          # Specify taxa coordinate correction.
                          mapping = aes_string(x = long,
                                               y = lat,
                                               label = "label"),
                          colour = color_opt, label.size = 0.1, 
                          size = 2.75, alpha = 0.75)
  return(phylo_geom)
}