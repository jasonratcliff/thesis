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

test_that("Consisent names and classes for SEINet occurrence data.", {
  if (is_checking()) {
    skip("Interactive testing for `seinet_coords` refactoring.")
  }

  intersection <- intersect(names(seinet), names(vouchers))

  # Verify class casting of SEINet today relative to `vouchers` intersection.
  expect_true(
    purrr::map2_lgl(
      .x = vouchers[, intersection],
      .y = seinet[, intersection],
      .f = function(.vouchers, .seinet) {
        identical(class(.vouchers), class(.seinet))
      }
    ) |> all()
  )

  # Check expected synonyms from SEINet search terms.
  expect_equal(
    sort(unique(seinet$scientificName)),
    c(
      "Physaria bellii",
      "Physaria floribunda subsp. floribunda",
      "Physaria floribunda subsp. osterhoutii",
      "Physaria rollinsii"
    )
  )

  # Verify filtering to records with geographic coordinates.
  expect_true(all(!is.na(seinet$decimalLongitude)))
  expect_true(all(!is.na(seinet$decimalLatitude)))
})
