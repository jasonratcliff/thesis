bounds <- list(x = c(-115, -100), y = c(36, 50))
bbox <- c(bounds$x[1], bounds$y[1], bounds$x[2], bounds$y[2])

records <- tibble::tibble(
  decimalLongitude = seq(bounds$x[1], bounds$x[2], length.out = 10),
  decimalLatitude = seq(bounds$y[1], bounds$y[2], length.out = 10),
  organismName = c(rep(LETTERS[1:3], each = 3), NA)
)

test_that("initialize class instance from `tbl_df` records", {
  extent <- Extent$new(records = vouchers)
  expect_s3_class(extent, c("Extent", "R6"))
  expect_s3_class(extent$records, c("tbl_df", "tbl", "data.frame"))
})

test_that("require tibble class inheritance for input data frame", {
  expect_error(
    Extent$new(records = as.data.frame(records)),
    regexp = "inherit.+tbl_df"
  )
})

test_that("prepend unique collection identifier if missing", {
  expect_message(
    assign("extent", Extent$new(records = records), envir = environment()),
    regexp = "collectionID"
  )

  expect_identical(names(extent$records)[1], "collectionID")
  expect_identical(extent$records$collectionID, seq_len(nrow(extent$records)))
})

test_that("raise error for duplicate / missing identifiers", {
  # TODO Understand global object vs. test environment for local assignment.
  records <<- records |>
    tibble::add_column(collectionID = seq_len(nrow(records)), .before = 1)

  records$collectionID[2] <- 1 # Duplicate record ID
  expect_error(Extent$new(records = records), regexp = "collectionID")

  records$collectionID[2] <- NA # Missing record ID
  expect_error(Extent$new(records = records), regexp = "collectionID")
})

# Verify collection IDs in object assignment to global environment.
if (expect_identical("collectionID" %in% names(records), TRUE)) {
  expect_identical(records$collectionID, seq_len(nrow(records)))
}

extent <- Extent$new(records = records)

test_that("default coordinate reference system", {
  skip("Questioning projection with geodetic CRS.")
  expect_identical(extent$crs, sf::st_crs("EPSG:4326"))
  expect_equal(sf::st_crs(extent$bbox())$input, "EPSG:4326")
})

test_that("get simple features bounding box", {
  names(bbox) <- c("xmin", "ymin", "xmax", "ymax")
  expect_s3_class(extent$bbox(), "bbox")
  expect_mapequal(extent$bbox(), bbox)
})

test_that("set bounding box to filter records", {
  extent <- extent$clone()
  coords <- sf::st_coordinates(extent$sf)
  i <- nrow(coords)

  # Verify coordinates prior to reducing size of bounding.
  expect_gte(coords[1, "X"], bounds$x[1])
  expect_gte(coords[1, "Y"], bounds$y[1])
  expect_lte(coords[i, "X"], bounds$x[2])
  expect_lte(coords[i, "Y"], bounds$y[2])

  extent$bbox(
    xmin = bounds$x[1] + 1,
    ymin = bounds$y[1] + 1,
    xmax = bounds$x[2] - 1,
    ymax = bounds$y[2] - 1
  )

  coords <- sf::st_coordinates(extent$sf)
  i <- nrow(coords)

  # Coordinate intersection with updated bounding box.
  expect_gt(coords[1, "X"], bounds$x[1])
  expect_gt(coords[1, "Y"], bounds$y[1])
  expect_lt(coords[i, "X"], bounds$x[2])
  expect_lt(coords[i, "Y"], bounds$y[2])
})

# TODO Consider stricter handling for non-scalar inputs or name mismatches!
test_that("warning for un-matched bbox names", {
  extent <- extent$clone()

  expect_error(
    extent$bbox(longitude = c(-110, -105), latitude = c(40, 45)),
    regexp = "list'.+coerced to type 'double"
  )

  expect_warning(
    extent$bbox(x1 = -110, y1 = 40, x2 = 40, y2 = 45),
    regexp = "Un-used arguments:.+x1.+y1.+x2.+y2"
  )

  expect_equal(as.numeric(extent$bbox()), bbox) # Verify un-changed bbox
})
