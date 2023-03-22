test_that("Specimen R6 Superclass", {
  vouchers <- tibble::tibble(
    Taxon = c(
      paste("Custodis", rep("impavidus", 2)),
      paste("Custodis", rep("'mundus'", 2)),
      paste("Medicari", rep("iugerum", 2)),
      "Medicari profundus ssp. longaevus"
    ),
    Collector = c(rep("A", 4), rep("B", 2), "C"),
    Collection_Number = 1:7,
    Date = as.Date((Sys.Date() - 6):Sys.Date(), origin = "1970-01-01"),
    # Expect 8 variations of herabrium codes.
    Herbarium = c("H", "H-1", "H- 1", "H -1", "[H-1], H", "H  ", "H-H"),
    Latitude = seq(from = 41, to = 44, length.out = 7),
    Longitude = seq(from = -107, to = -105, length.out = 7)
  ) %>%
    dplyr::mutate(
      label = dplyr::case_when(
        Collection_Number == 1 ~ NA_character_,
        TRUE ~ .data$Taxon
      )
    ) %>%
    Specimen$new(
      records = .,
      identifier = "Taxon"
    )

  expect_identical(class(vouchers), c("Specimen", "R6"))
  expect_identical(class(vouchers$records), c("tbl_df", "tbl", "data.frame"))
  purrr::walk(
    # TODO Check public functions class - metaprogramming with R6 `$` closure?
    .x = list(
      vouchers$census,
      vouchers$limit,
      vouchers$taxa,
      vouchers$collections
    ),
    .f = function(public) {
      expect_type(public, "closure")
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
  voucher_collections <- vouchers$collections("A" = c(1, 2), 5, "C")
  expect_identical(class(voucher_collections), c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(voucher_collections), 4)
  expect_equal(nrow(vouchers$records), 7)

  # Annotaions Labels ----------------------------------------------------------
  expect_equal(
    vouchers$annotations(),
    list(
      "Custodis impavidus" = "*Custodis* *impavidus*",
      "Custodis 'mundus'" = "*Custodis* 'mundus'",
      "Medicari iugerum" = "*Medicari* *iugerum*",
      "Medicari profundus ssp. longaevus" = paste(
        "*Medicari* *profundus*",
        "<br><span> &nbsp;&nbsp;&nbsp; </span>",
        "ssp. *longaevus*"
      )
    )
  )
  expect_equal(
    vouchers$labels(),
    c(
      NA_character_, "italic(C.~impavidus)",
      rep("italic(C.)~\"'mundus'\"", 2),
      rep("italic(M.~iugerum)", 2),
      "italic(M.~p.)~ssp.~italic(longaevus)"
    )
  )
})
