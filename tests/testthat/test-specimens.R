## ---- Specimens$new() --------------------------------------------------------

test_that("initialize `Specimen` object from `tbl_df` records", {
  x <- Specimen$new(records = vouchers)
  expect_s3_class(x, c("Specimen", "R6"))
  expect_s3_class(x$records, c("tbl_df", "tbl", "data.frame"))
})

test_that("raise error if input does not inherit `tbl_df` class", {
  expect_error(
    Specimen$new(records = as.data.frame(vouchers)),
    regexp = "data.frame"
  )
})

## ---- Specimen$identifier ----------------------------------------------------

test_that("new object is set with default DWC term", {
  x <- Specimen$new(records = vouchers)
  expect_identical(x$identifier, "scientificName")
})

test_that("initialize with valid non-default $identifier active field", {
  x <- Specimen$new(records = vouchers, identifier = "organismName")
  expect_identical(x$identifier, "organismName")
  expect_error(
    Specimen$new(records = vouchers, identifier = "nonDarwinCore"),
    regexp = "`identifier`"
  )
})

test_that("update $identifier active field with valid term", {
  x <- Specimen$new(records = vouchers)
  x$identifier <- "organismName"
  expect_identical(x$identifier, "organismName")
})

test_that("error for non-character scalars or vectors ≥ length 1", {
  x <- Specimen$new(records = thesis::vouchers)
  expect_error(x$identifier <- 1859)
  expect_error(x$identifier <- as.symbol("scientificName"))
  expect_error(x$identifier <- c("scientificName", "organismName"))
  expect_error(x$identifier <- "missingTerm")
})

## ---- Specimens$census() -----------------------------------------------------
test_that(
  desc = "Record census", # TODO Improve print | summarization method
  code = {
    specimens <- build_specimens()$clone()
    expect_equal(
      object = specimens$census(),
      expected = list(total = 8, distinct = 7)
    )
  }
)

## ---- Specimens$filter_limit() -----------------------------------------------
test_that(
  desc = "Filter specimen records by coordinate limits from cardinal headings.",
  code = {
    specimens <- build_specimens()$clone()

    # Expect default NULL values to return unchanged `self$records` tibble.
    expect_identical(specimens$records, specimens$filter_limit(.return = TRUE))

    # Check silent update for default `.return` = FALSE.
    filtered <- specimens$clone()
    purrr::iwalk(
      .x = list(west = -106.75, south = 41.75, east = -105.25, north = 43.25),
      .f = function(heading, name) {
        row <- nrow(filtered$records)
        args <- rlang::list2({{ name }} := heading, .return = FALSE)
        rlang::eval_tidy(
          expr = {
            rlang::call2(.fn = filtered$filter_limit, !!!args)
          }
        )
        expect_lt(nrow(filtered$records), row)
      }
    )

    # Check expected return tibble row counts for filtered limits.
    purrr::pwalk(
      .l = list(
        heading = c("west", "south", "east", "north"),
        coordinate = c(-106.75, 42, -105.5, 43.5),
        reference = rep(c("decimalLongitude", "decimalLatitude"), times = 2),
        comparison = c(rep(">", 2), rep("<", 2))
      ),
      .f = function(heading, coordinate, reference, comparison) {
        args <- rlang::list2({{ heading }} := coordinate, .return = TRUE)
        filtered <- rlang::eval_tidy(
          expr = {
            rlang::call2(.fn = specimens$filter_limit, !!!args)
          }
        )
        compared <-
          switch(
            EXPR = comparison,
            ">" = expect_gt(min(filtered[[reference]]), coordinate),
            "<" = expect_lt(max(filtered[[reference]]), coordinate)
          )
      }
    )
  }
)

## ---- Specimens$filter_taxa() ------------------------------------------------
test_that(
  desc = "Filter specimen records by taxonomic annotation from identifications",
  code = {
    specimens <- build_specimens()$clone()

    # Check silent update for default `.return` = FALSE.
    purrr::walk2(
      .x = list("Medicari" = c("iugerum", "longaevus"), "impavidus"),
      .y = c(3, 2),
      .f = function(taxon, count) {
        filtered <- specimens$clone()
        filtered$filter_taxa(taxon, .return = FALSE)
        expect_equal(nrow(filtered$records), count)
      }
    )

    # Check expected return tibble row counts for filtered taxa.
    purrr::walk2(
      .x = c("Custodis", "Medicari"),
      .y = c(4, 3),
      .f = function(taxa, count) {
        filtered <- specimens$filter_taxa(unlist(taxa), .return = TRUE)
        expect_equal(nrow(filtered), count)
      }
    )
  }
)

## ---- Specimens$filter_collections() -----------------------------------------
test_that(
  desc = "Filter specimen records by botanical collector and collection number.",
  code = {
    specimens <- build_specimens()$clone()

    # Check silent update for default `.return` = FALSE.
    purrr::walk2(
      .x = list(c("A", "B"), 5:7, list("B" = 5:6, "C" = 7)),
      .y = c(6, 3, 3),
      .f = function(search, count) {
        filtered <- specimens$clone()
        filtered$filter_collections(!!!search, .return = FALSE)
        expect_equal(nrow(filtered$records), count)
      }
    )

    # Check expected return tibble row counts for filtered collections.
    purrr::walk2(
      .x = list(1:7, c("A", "B", "C"), list("B" = 5:6), c("C" = 7)),
      .y = c(7, 7, 2, 1),
      .f = function(search, count) {
        filtered <- specimens$filter_collections(!!!search, .return = TRUE)
        expect_equal(nrow(filtered), count)
      }
    )
  }
)

## ---- Specimens$annotations() ------------------------------------------------
test_that(
  desc = "Return vector of specimen annotations for manual scale values.",
  code = {
    specimens <- build_specimens()$clone()
    expect_equal(
      specimens$annotations(),
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
  }
)

## ---- Specimens$labels() -----------------------------------------------------
test_that(
  desc = "Return vector for layering plot labels.",
  code = {
    specimens <- build_specimens()$clone()
    expect_equal(
      specimens$labels(),
      c(
        NA_character_, "italic(C.~impavidus)",
        rep("italic(C.)~\"'mundus'\"", 2),
        rep("italic(M.~iugerum)", 2),
        "italic(M.~p.)~ssp.~italic(longaevus)"
      )
    )
  }
)
