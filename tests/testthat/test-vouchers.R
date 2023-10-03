test_that("Replace specimens data variables with Darwin Core terms.", {
  if (is_checking()) {
    skip("Interactive testing for `herbarium_specimens` refactoring.")
  }

  # Compare updated prior identification parsing; account for ID qualifier.
  expect_true(
    purrr::map2_lgl(
      .x = vouchers$scientificName,
      .y = herbarium_specimens$Taxon_a_posteriori,
      .f = \(x, y) identical(x, y)
    ) |> all()
  )

  # Compare parsed elevation data accounting for conversion to meters.
  expect_true(
    purrr::map2_lgl(
      .x = herbarium_specimens$Elev_raw_min,
      .y = vouchers$minimumElevationInMeters,
      .f = function(Elev_raw_min, minimumElevationInMeters) {
        if (identical(minimumElevationInMeters, Elev_raw_min)) return(TRUE)
        near(minimumElevationInMeters, (Elev_raw_min / 3.281), tol = 2)
      }
    ) |> all()
  )
})
