# Specimen Class ---------------------------------------------------------------

#' @title R6 Superclass Encapsulating Voucher Specimen Records
#'
#' @description
#' The `Specimen` superclass facilitates analysis of voucher subsets by
#' instantiating a data structure with public methods for filtering and
#' annotating specimens. A core aspect of this class is the `Specimen$records`
#' public field, which contains a [tibble::tibble()] data frame meant to
#' represent specimen records. Plotting  subsets is enabled by superclass
#' methods to filter by geographic coordinates, taxonomic annotation, and
#' collection information (i.e., collector and number). Visualization of
#' filtered specimen distributions is facilitated by methods to format
#' annotations for plot labels.
#'
#' @seealso [thesis::SpecimenMap]
#' @param records Specimen voucher records [tibble::tibble()].
#' @param identifier Character scalar for voucher annotation label variable.
#' @param .identifier Character scalar denoting `records` field variable
#'  for filtering and annotation operations involving taxonomic identifications.
#' @param .return Logical to return filtered records. By default, the public
#'   field `Specimen$records` is updated silently to allow chaining subsequent
#'   filtering operations. When `TRUE`, return the filtered records without
#'   updating the public field.
#' @include extent.R
#' @export
Specimen <- R6::R6Class(
  classname = "Specimen",
  inherit = Extent,
  private = list(
    .identifier = NULL
  ),
  active = list(
    #' @field identifier Character scalar denoting a default variable in
    #'   the `Specimen$records` field. For records with a `.identifier`
    #'   argument, this field is referenced when the optional argument is
    #'   omitted. The `identifier` field is used for operations involving a
    #'   specific set of taxonomic identifications (e.g., prior annotations),
    #'   including filtering and labelling methods.
    identifier = function(value) {
      if (missing(value)) {
        private$.identifier
      } else {
        stopifnot(
          all(
            is.character(value), length(value) == 1,
            value %in% names(private$.records)
          )
        )
        private$.identifier <- value
        invisible(self)
      }
    }
  ),
  public = list(
    #' @description Construct a `Specimen` class container from voucher records.
    #'
    #' @examples
    #' # Initialize object instance from set of records with `new()` method.
    #' specimens <- Specimen$new(
    #'   records = thesis::vouchers,
    #'   identifier = "scientificName"
    #' )
    #'
    #' class(specimens)
    #'
    #' # Specimen records can be readily accessed from public `records` field.
    #' class(specimens$records)
    #'
    #' dim(specimens$records)
    initialize = function(records, identifier = NULL) {
      super$initialize(records)
      if (is.null(identifier)) {
        private$.identifier <- "scientificName"
      } else {
        if (!identifier %in% names(private$.records)) {
          rlang::abort(c("`identifier` must match one of:", dwc))
        } else {
          private$.identifier <- identifier
        }
      }
    },
    #' @description Record census accounting of voucher specimens.
    #'
    #' @details
    #' Reports two summations:
    #'  * Total number of specimens **including duplicate collections**
    #'  * Number of *distinct* specimen collections (i.e., excluding duplicates)
    #'
    #' @examples
    #' # Check basic record census of total and distinct specimens.
    #' specimens$census()
    census = function() {
      vouchers <- self$records %>%
        dplyr::select(
          "recordedBy", "recordNumber",
          "eventDate", "institutionCode"
        )
      record_census <-
        list(
          total = dplyr::mutate(
            .data = vouchers,
            original = institutionCode,
            institutionCode = stringr::str_remove_all(
              string = .data$institutionCode,
              pattern = "[\\[\\]]|([ \\-]?[0-9]+)|( +$)"
            )
          ) %>%
            tidyr::separate_rows(institutionCode, sep = ", ") %>%
            nrow(),
          distinct = dplyr::mutate(
            .data = vouchers,
            row_id = seq_len(nrow(vouchers)),
            institutionCode = stringr::str_remove_all(
              string = .data$institutionCode,
              pattern = "\\[|\\]"
            ) %>%
              stringr::str_split(string = ., pattern = ", +") %>%
              purrr::map_chr(., function(herbarium) {
                herbarium_split <- sort(unlist(herbarium)) %>%
                  stringr::str_c(., collapse = " ")
                ifelse(length(herbarium_split) == 1, herbarium_split, "NA")
              })
          ) %>%
            dplyr::distinct(
              .data$recordedBy,
              .data$recordNumber,
              .data$eventDate,
              .data$institutionCode,
              .keep_all = TRUE
            ) %>%
            nrow()
        )
      return(record_census)
    },
    #' @description Filter specimen records by taxonomic annotations.
    #'
    #' @details
    #' Given a \href{#public-fields}{\code{Specimen$records}} tibble, utilize
    #' *partial* string matching for taxonomic filtering of specimen records.
    #' Flexibility is provided to query for e.g., multiple specific epithets.
    #'
    #' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Taxonomic terms to filter
    #'  records against column designation by `.identifier` parameter.
    #'
    #' @examples
    #' # Filter specimens by taxonomic rank; here, reviewed specific epithets.
    #' clone <- specimens$clone()
    #' clone$filter_taxa(
    #'   c("acutifolia", "floribunda", "vitulifera", "medicinae"),
    #'   .identifier = "scientificName"
    #' )
    #' dim(clone$records)
    #'
    #' # Further subset filtered records and return a separate tibble.
    #' filtered <-
    #'   clone$filter_taxa(
    #'     "vitulifera",
    #'     .identifier = "previousIdentifications",
    #'     .return = TRUE
    #'   )
    #' dim(filtered)
    filter_taxa = function(..., .identifier = NULL, .return = FALSE) {
      species <- rlang::list2(...) %>%
        purrr::flatten() %>%
        purrr::flatten_chr()
      .identifier <- .identifier %||% self$identifier
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
        private$.filtered <- filtered
        invisible(self)
      }
    },
    #' @description Filter specimen records by collector or collection number.
    #'
    #' @details
    #' This method uses dot collection via [rlang::dots_list()], allowing for
    #' flexible specification of various collector and collection combos.
    #' Input arguments should take one of the following forms:
    #' * Numeric vector of collection numbers named by collector
    #' * Character scalar specifying a single collector string
    #' * Numeric scalar specifying a single collection number
    #'
    #' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Any number of collector
    #'  collections (i.e., numeric vector named by collector), collectors
    #'  (character scalars) or collection numbers (numeric scalars).
    #'
    #' @examples
    #' clone <- specimens$clone()
    #' clone$filter_collections(5068, "Rollins" = c(5145, 5146), "Mulligan")
    #' clone$census()
    #'
    #' clone$records[, c("recordedBy", "recordNumber")]
    filter_collections = function(..., .return = FALSE) {
      search <- rlang::dots_list(..., .named = TRUE)
      filtered <- purrr::imap_dfr(
        .x = search,
        .f = function(query, name) {
          if (length(query) == 1) {
            if (is.numeric(query)) {
              collections <- self$records %>%
                dplyr::filter(
                  grepl(pattern = query, x = recordNumber)
                )
              return(collections)
            } else if (is.character(query)) {
              collections <- self$records %>%
                dplyr::filter(grepl(pattern = query, x = recordedBy))
              return(collections)
            }
          } else {
            collections <- self$records %>%
              dplyr::filter(
                grepl(pattern = name, x = recordedBy),
                grepl(
                  pattern = paste(query, collapse = "|"),
                  x = .data$recordNumber
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
        private$.filtered <- filtered
        invisible(self)
      }
    },
    #' @description Create markdown-formatted specimen annotations.
    #'
    #' @details
    #' Used for plotting [ggplot2::ggplot()] manual scale values with italicized
    #' fonts via the [ggtext::element_markdown()] extension.
    #'
    #' @return Named list of markdown- (i.e., HTML) formatted annotations.
    #'  Length equals number of unique values in the `.identifier` field.
    #'
    #' @examples
    #' # Omit `.identifier` for default set by public field `self$identifier`.
    #' clone <- specimens$clone()
    #' clone$filter_taxa("didymocarpa")
    #'
    #' # Use a different records tibble column to create named annotations.
    #' clone$annotations(.identifier = "organismName")
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
    #'
    #' @examples
    #' \dontrun{
    #'   # TODO Superclass raises error for records missing columns:
    #'   #   > decimalLongitude; decimalLatitude
    #'   dna_vouchers <- dplyr::select(thesis::dna_specimens, ID_final) %>%
    #'    dplyr::rename(label = ID_final) %>%
    #'    Specimen$new(
    #'      records = .,
    #'      identifier = "ID_final"
    #'    )
    #'
    #'   # Note length of the returned label vector equals the record row number.
    #'   length(dna_vouchers$labels())
    #'   unique(dna_vouchers$labels())
    #' }
    labels = function(.identifier = "label") {
      if (is.null(.identifier)) .identifier <- self$identifier
      labels <- self$records[[.identifier]] %>%
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
