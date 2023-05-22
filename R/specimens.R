# Specimen Class ---------------------------------------------------------------

#' @title Voucher Specimens
#' @description
#' R6 class defined as a specimen record data structure. The [Specimen]
#' superclass holds a [`tibble`][tibble::tibble()] object containing voucher
#' metadata. Public methods allow for reporting the number of distinct and
#' duplicate records, subsetting specimen records by geographic coordinate,
#' and filtering operations by either annotation or collector / collection.
#'
#' @param records Specimen voucher records [tibble::tibble()].
#' @param identifier Character scalar for voucher annotation label variable.
#' @export
Specimen <- R6::R6Class(
  classname = "Specimen",
  public = list(

    #' @field records A [`tbl_df`][tibble::tbl_df-class] S3 tibble object
    #'  with set of specimen vouchers.
    records = "tbl_df",

    #' @field identifier Annotation variable in a [`Specimen$records`][Specimen]
    #'  tibble. Used to designate [ggplot2::ggplot()] scale labels.
    identifier = NULL,

    #' @description Construct a `Specimen` container
    #' @examples
    #' # Construct object instance with `$new()` method
    #' voucher <- Specimen$new(
    #'   records = thesis::herbarium_specimens,
    #'   identifier = "Taxon_a_posteriori"
    #' )
    #'
    #' # Basic access to the public `records` field tibble
    #' class(voucher$records)
    #' dim(voucher$records)
    initialize = function(records, identifier) {
      self$records <- records
      self$identifier <- identifier
    },

    #' @description Record census accounting of voucher specimens.
    #' Reports two summations:
    #'  * Total number of specimens **including duplicate collections**
    #'  * Number of *distinct* specimen collections (i.e., excluding duplicates)
    #' @examples
    #' # Check basic record census of total and distinct specimens.
    #' voucher$census()
    census = function() {
      vouchers <- self$records %>%
        dplyr::select(
          "Collector", "Collection_Number",
          "Date", "Herbarium"
        )
      record_census <-
        # tibble::tibble(
        list(
          total = dplyr::mutate(
            .data = vouchers,
            original = Herbarium,
            Herbarium = stringr::str_remove_all(
              string = .data$Herbarium,
              pattern = "[\\[\\]]|([ \\-]?[0-9]+)|( +$)"
            )
          ) %>%
            tidyr::separate_rows(Herbarium, sep = ", ") %>%
            nrow(),
          distinct = dplyr::mutate(
            .data = vouchers,
            row_id = 1:nrow(vouchers),
            Herbarium = stringr::str_remove_all(
              string = .data$Herbarium,
              pattern = "\\[|\\]"
            ) %>%
              stringr::str_split(string = ., pattern = ", +") %>%
              purrr::map_chr(., function(herbarium) {
                herbarium_split <- sort(unlist(herbarium)) %>%
                  stringr::str_c(., collapse = " ")
                ifelse(length(herbarium_split) == 1, herbarium_split, "NA")
              })
          ) %>%
            dplyr::distinct(., .data$Collector, .data$Collection_Number,
              .data$Date, .data$Herbarium,
              .keep_all = TRUE
            ) %>%
            nrow()
        )
      return(record_census)
    },

    #' @description Limit records by geographic coordinate (lon/lat).
    #' This method enables filtering records by minimum or maximum
    #' coordinate limits using any combination of the four cardinal directions.
    #' Each parameter sets the directional bound (min/max) to filter.
    #' @param west Filter records by **minimum longitude** (min. x)
    #' @param east Filter records by **maximum longitude** (max. x)
    #' @param south Filter records by **minimum latitude** (min. y)
    #' @param north Filter records by **maximum latitude** (max. y)
    #' @examples
    #' # Subset records by geographic coordinates
    #' limits <- voucher$clone()
    #' limits$limit(west = -107, east = -105, south = 39, north = 41)
    #'
    #' dim(limits$records)
    limit = function(west = NULL, south = NULL, east = NULL, north = NULL,
                     .return = FALSE) {
      headings <- tibble::tibble(
        heading = c("west", "south", "east", "north"),
        reference = rep(c("Longitude", "Latitude"), times = 2),
        comparison = c(rep(">", 2), rep("<", 2)),
        coordinate = purrr::map_dbl(
          .x = list(west, south, east, north),
          .f = function(heading) {
            ifelse(
              test = !is.null(heading),
              yes = heading,
              no = NA_real_
            )
          }
        )
      ) %>%
        dplyr::filter(!is.na(coordinate))

      if (!nrow(headings) > 0) {
        usethis::ui_oops("At least one meridian or parallel recommended:")
        purrr::walk(
          .x = c("west", "south", "east", "north"),
          .f = ~ print(glue::glue("#> {usethis::ui_field(.x)}"))
        )
        usethis::ui_info("Returning object unfiltered by coordinate limit.")
        return(self$records)
      } else {
        cardinals <- purrr::pmap(
          .l = headings,
          .f = function(heading, reference, comparison, coordinate) {
            limit <-
              glue::glue(
                "dplyr::filter(.data[['{reference}']] {comparison} {coordinate})"
              )
            str2lang(limit)
          }
        )
      }
      filtered <- rlang::eval_tidy(
        expr = {
          purrr::reduce(
            .x = unlist(cardinals),
            .f = ~ rlang::expr(!!.x %>% !!.y),
            .init = rlang::expr(self$records)
          )
        }
      )
      if (.return) {
        return(filtered)
      } else {
        self$records <- filtered
        invisible(self)
      }
    },

    #' @description
    #' Given a \href{#public-fields}{\code{Specimen$records}} tibble, utilize
    #' *partial* string matching for taxonomic filtering of specimen records.
    #' Flexibility is provided to query for e.g., multiple specific epithets.
    #' @param identifier `records` field tibble variable for regular expression
    #'  matching by [dplyr::filter()].
    #' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Taxonomic terms to filter
    #'  records against column designation by `identifier` parameter.
    #' @examples
    #' # Filter specimens by taxonomic rank; here, reviewed specific epithets.
    #' species <- voucher$clone()
    #' species$taxa(c("flori", "medi"), identifier = "Taxon_a_posteriori")
    #'
    #' # Index tibble records column annotations to tabulate.
    #' table(species$records[["Taxon_a_posteriori"]])
    taxa = function(..., .identifier = NULL, .return = FALSE) {
      species <- rlang::list2(...) %>%
        purrr::flatten() %>%
        purrr::flatten_chr()
      if (is.null(.identifier)) .identifier <- self$identifier
      filtered <- self$records %>%
        dplyr::filter(
          grepl(
            pattern = paste(species, collapse = "|"),
            x = .data[[.identifier]]
          )
        )
      if (.return) {
        return(filtered)
      } else {
        self$records <- filtered
        invisible(self)
      }
    },

    #' @description Query specimen records by collector or collection number.
    #' This method uses dot collection via [rlang::dots_list()], allowing for
    #' flexible specification of various collector and collection combos.
    #' Input arguments should take one of the following forms:
    #' * Numeric vector of collection numbers named by collector
    #' * Character scalar specifying a single collector string
    #' * Numeric scalar specifying a single collection number
    #' @return A [`tbl_df`][tibble::tbl_df-class] subset of matched
    #'  collector / collection specimen records.
    #' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Any number of collector
    #'  collections (i.e., numeric vector named by collector), collectors
    #'  (character scalars) or collection numbers (numeric scalars).
    #' @examples
    #' found <- voucher$clone()
    #' found$collections(5068, "Rollins" = c(5145, 5146), "Mulligan")
    #' found$census()
    #'
    #' dplyr::select(found$records, "Collector", "Collection_Number")
    collections = function(..., .return = FALSE) {
      search <- rlang::dots_list(..., .named = TRUE)
      filtered <- purrr::imap_dfr(
        .x = search,
        .f = function(query, name) {
          if (length(query) == 1) {
            if (is.numeric(query)) {
              collections <- self$records %>%
                dplyr::filter(
                  grepl(pattern = query, x = Collection_Number)
                )
              return(collections)
            } else if (is.character(query)) {
              collections <- self$records %>%
                dplyr::filter(grepl(pattern = query, x = Collector))
              return(collections)
            }
          } else {
            collections <- self$records %>%
              dplyr::filter(
                grepl(pattern = name, x = Collector),
                grepl(
                  pattern = paste(query, collapse = "|"),
                  x = .data$Collection_Number
                )
              )
            return(collections)
          }
        }
      ) %>%
        purrr::keep(~ !is.null(.x))
      if (.return) {
        return(filtered)
      } else {
        self$records <- filtered
        invisible(self)
      }
    },

    #' @description Create markdown-formatted specimen annotations.
    #' Used for plotting [ggplot2::ggplot()] manual scale values with italicized
    #' fonts via the [ggtext::element_markdown()] extension.
    #'
    #' @return Named list of markdown- (i.e., HTML) formatted annotations.
    #'  Length equals number of unique values in the `identifier` field.
    #' @examples
    #' limits$annotations()
    annotations = function(.identifier = NULL) {
      if (is.null(.identifier)) .identifier <- self$identifier
      annotations <- unique(self$records[[.identifier]]) %>%
        rlang::set_names() %>%
        purrr::map(
          .x = .,
          .f = function(taxon) {
            stringr::str_squish(taxon) %>%
              stringr::str_split(string = ., pattern = " ") %>%
              purrr::map_chr(
                .x = .,
                .f = function(rank) {
                  dplyr::case_when(
                    grepl(pattern = "^[A-z][a-z-]+$", x = rank) ~
                      paste0("*", rank, "*"),
                    grepl(pattern = "^\\'[A-z]+\\'$", x = rank) ~ rank, # quoted
                    grepl(pattern = "^[-?]$", x = rank) ~ rank, # questioned
                    grepl(pattern = "^(subsp|ssp|var)\\.?$", x = rank) ~
                      paste(
                        "<br><span>",
                        paste0(rep("&nbsp;", 3), collapse = ""),
                        "</span>", rank
                      ),
                    TRUE ~ as.character(glue::glue("Unmatched case for: {rank}"))
                  ) %>%
                    paste(., collapse = " ")
                }
              )
          }
        )
      return(annotations)
    },

    #' @description Create base expression specimen labels.
    #'
    #' @return Character vector of expressions for parsed font labels.
    #'  Length equivalent to length of the records `label` variable.
    #' @examples
    #' dna_vouchers <- dplyr::select(thesis::dna_specimens, ID_final) %>%
    #'  dplyr::rename(label = ID_final) %>%
    #'  Specimen$new(
    #'    records = .,
    #'    identifier = "ID_final"
    #'  )
    #'
    #' # Note length of the returned label vector equals the record row number.
    #' length(dna_vouchers$labels())
    #' unique(dna_vouchers$labels())
    labels = function() {
      labels <- self$records[["label"]] %>%
        stringr::str_squish() %>%
        stringr::str_replace_all(
          string = .,
          pattern = "[ _]+(?=[A-z\\.\\'])",
          replacement = " "
        ) %>%
        purrr::map_chr(
          .x = .,
          .f = function(label) {
            if (!is.na(label)) {
              taxa_ranks <-
                unlist(stringr::str_split(string = label, pattern = " "))
              if (length(taxa_ranks) == 2) {
                if (grepl(pattern = "^\\'[A-z]+\\'$", x = taxa_ranks[2])) {
                  glue::glue(
                    "italic(",
                    abbreviate(taxa_ranks[1], dot = TRUE, minlength = 1),
                    ")~\"{ taxa_ranks[2] }\""
                  )
                } else {
                  glue::glue(
                    "italic(",
                    abbreviate(taxa_ranks[1], dot = TRUE, minlength = 1),
                    "~{ taxa_ranks[2] })"
                  )
                }
              } else {
                if (length(taxa_ranks) == 4 &
                  grepl(pattern = "^(subsp|ssp|var)\\.?$", x = taxa_ranks[3])) {
                  glue::glue(
                    "italic(",
                    abbreviate(taxa_ranks[1], dot = TRUE, minlength = 1),
                    "~",
                    abbreviate(taxa_ranks[2], dot = TRUE, minlength = 1),
                    ")~{ taxa_ranks[3] }~italic({ taxa_ranks[4] })"
                  )
                }
              }
            } else {
              return(NA_character_)
            }
          }
        )
      return(labels)
    }
  )
)
