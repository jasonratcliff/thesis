# Private method $geoms() returns list of simple features layers

    Code
      private$geoms()[[3]][[1]]$mapping
    Output
      Aesthetic mapping: 
      * `colour` -> `.data[["scientificName"]]`
      * `shape`  -> `.data[["scientificName"]]`

# Private method $theme() returns theme options

    Code
      private$theme()[[2]]
    Output
      $x
      [1] "decimalLongitude"
      
      $y
      [1] "decimalLatitude"
      
      $colour
      [1] "scientificName"
      
      $shape
      [1] "scientificName"
      
      attr(,"class")
      [1] "labels"

