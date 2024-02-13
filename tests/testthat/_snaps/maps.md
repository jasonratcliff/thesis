# Specimens jitter layer

    Code
      specimen_map$specimens
    Output
      mapping: x = ~decimalLongitude, y = ~decimalLatitude, colour = ~.data[["scientificName"]], shape = ~.data[["scientificName"]] 
      geom_point: na.rm = FALSE
      stat_identity: na.rm = FALSE
      position_jitter 

