# Private method $geoms() returns list of simple features layers

    Code
      private$geoms()[[3]][[1]]$mapping
    Output
      Aesthetic mapping: 
      * `colour` -> `.data[["scientificName"]]`
      * `shape`  -> `.data[["scientificName"]]`

# SpecimenMap R6 Subclass

    Code
      voucher_theme[[2]]
    Output
      $x
      [1] "decimalLongitude"
      
      $y
      [1] "decimalLatitude"
      
      $colour
      [1] "scientificName"
      
      $shape
      [1] "scientificName"
      
      $size
      [1] "scientificName"
      
      attr(,"class")
      [1] "labels"

