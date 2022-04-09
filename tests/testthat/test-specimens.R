test_that("Specimen R6 Superclass", {
  vouchers <- tibble::tibble(
    Taxon = c(
      paste("Custodis", rep("impavidus", 2)),
      paste("Custodis", rep("mundus", 2)),
      paste("Medicari", rep("iugerum", 2)),
      "Medicari profundus"
    ),
    Collector = c(rep("A", 4), rep("B", 2), "C"),
    Collection_Number = 1:7,
    Date = as.Date((Sys.Date() - 6):Sys.Date(), origin = "1970-01-01"),
    # Expect 8 variations of herabrium codes.
    Herbarium = c("H", "H-1", "H- 1", "H -1", "[H-1], H", "H  ", "H-H"),
    Latitude = seq(from = 41, to = 44, length.out = 7),
    Longitude = seq(from = -107, to = -105, length.out = 7)
  ) %>%
    Specimen$new(records = .)

  expect_is(vouchers, class = c("Specimen", "R6"))
  expect_is(vouchers$records, class = c("tbl_df", "tbl", "data.frame"))
  purrr::walk(
    .x = list(
      vouchers$census,
      vouchers$limit,
      vouchers$taxa,
      vouchers$collections
    ),
    .f = function(public) {
      expect_is(public, class = "function")
    }
  )

  # Record Census --------------------------------------------------------------
  expect_equal(
    object = vouchers$census(),
    expected = list(total = 8, distinct = 7)
  )

  # Specimen Limits ------------------------------------------------------------
  voucher_limits <- vouchers$clone()
  voucher_limits$limit(west = -106, east = -105.5, south = 42, north = 43.5)
  expect_equal(nrow(voucher_limits$records), 1)

  # Taxa Filtering -------------------------------------------------------------
  voucher_taxa <- vouchers$clone()
  voucher_taxa$taxa("Medicari", "impavidus", identifier = "Taxon")
  expect_equal(nrow(voucher_taxa$records), 5)

  # Voucher Collections --------------------------------------------------------
  voucher_collections <- vouchers$clone()
  voucher_collections$collections("A", "B" = c(5, 6), "C" = 7)
  expect_equal(nrow(voucher_collections$records), 7)
})
