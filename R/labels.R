#' Specimen Voucher Labels
#'
#' Format occurrence data for `LaTeX` typesetting of voucher labels.
#' @export
Labels <- R6::R6Class(
  classname = "Labels",
  private = list(
    terms = c(
      "scientificName", "scientificAuthor",
      "stateProvince", "county", "verbatimLocality",
      "recordedBy", "recordNumber", "typeStatus",
      "decimalLatitude", "decimalLongitude",
      "verbatimElevation"
    ),
    preamble = function(.document = "article") {
      .document <- match.arg(arg = .document, choices = "article")
      glue::glue_safe("
        %% LaTeX Preamble
        \\documentclass{<<.document>>}
        \\usepackage[margin=0pt]{geometry}
        \\usepackage{multicol}
        \\setlength\\parindent{0pt}
        \\begin{document}
        ",
        .sep = "\n", .open = "<<", .close = ">>"
      )
    },
    label = function(.record) {
      stopifnot(inherits(.record, "tbl_df") & nrow(.record) == 1)
      collection <- .record |>
        dplyr::mutate(
          state = .record$stateProvince,
          taxa = .record$scientificName,
          author = .record$scientificAuthor,
          type = dplyr::if_else(
            condition = is.na(.record$typeStatus), true = "",
            false = glue::glue_data(.record, " \\hfill{{}} {typeStatus}")
          ),
          county = .record$county,
          latitude = .record$decimalLatitude,
          longitude = .record$decimalLongitude,
          locale = .record$verbatimLocality,
          # TODO Handle range of elevelations; unit conversion: m -> f
          elevation = .record$verbatimElevation,
          elev_unit = "ft. elev",
          opening = ifelse(
            # Open `minipage` nested with 2 column `multicol` environment.
            test = .record$rowId %% 2,
            yes = private$counter$open,
            no = ""
          ),
          closing = ifelse(
            # Handle even-numbered or final odd-numbered record to close.
            test = !(.record$rowId %% 2) |
              (.record$rowId == nrow(self$occurrences)),
            yes = private$counter$close,
            no = ""
          ),
          .keep = "unused"
        )
      glue::glue_data_safe(
        .x = collection, "
          <<opening>>
          \\section{Flora of <<state>>}

          \\textit{\\textbf{<<taxa>>}} <<author>><<type>>

          \\bigskip
          <<county>> County: <<locale>>
          \\newline
          <<latitude>>°N <<longitude>>°W \\hfill{} <<elevation>> <<elev_unit>>

          \\bigskip
          <<closing>>
          \\
          ",
        .open = "<<", .close = ">>",
        .sep = "\n"
      )
    },
    counter = {
      list(
        open = "
          \\begin{minipage}{\\linewidth}
          \\begin{multicols}{2}
        ",
        close = "
          \\end{multicols}
          \\end{minipage}
        "
      )
    }
  ),
  public = list(
    occurrences = NULL,
    initialize = function(records) {
      self$occurrences <- records |>
        dplyr::select(dplyr::all_of(private$terms)) |>
        dplyr::mutate(
          rowId = dplyr::row_number()
        )
    },
    tex = function() {
      # Map *p-variables* from *n-records* of specimen occurrences.
      .labels <- purrr::pmap(
        .l = self$occurrences,
        .f = function(...) {
          l <- rlang::dots_list(..., .named = TRUE)
          private$label(.record = tibble::as_tibble(l))
        }
      )
      composed <-
        list(
          preamble = private$preamble(),
          labels = .labels,
          ending = "\\end{document}"
        )
      return(composed)
    },
    pdf = function(.write = FALSE) {
      if (.write) stop("Unimplemented")
      .NotYetImplemented()
    }
  )
)
