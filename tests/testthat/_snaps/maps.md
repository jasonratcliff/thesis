# SpecimenMap R6 Subclass

    Code
      voucher_specimens[[1]]$mapping
    Output
      Aesthetic mapping: 
      * `x`      -> `decimalLongitude`
      * `y`      -> `decimalLatitude`
      * `colour` -> `.data[["scientificName"]]`
      * `shape`  -> `.data[["scientificName"]]`

---

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

