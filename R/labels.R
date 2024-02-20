#' Specimen Voucher Labels
#'
#' Format occurrence data for `LaTeX` typesetting of voucher labels.
#' @export
Labels <- R6::R6Class(
  classname = "Labels",
  private = list(
    terms = {
      c(
        "scientificName", "institutionCode",
        "stateProvince", "county", "verbatimLocality",
        "recordedBy", "recordNumber", "typeStatus",
        "decimalLatitude", "decimalLongitude",
        "verbatimElevation",
        "habitat", "associatedTaxa",
        "occurrenceRemarks"
      )
    },
    preamble = function(.document = "article") {
      .document <- match.arg(arg = .document, choices = "article")
      glue::glue_safe("
        %% LaTeX Preamble
        \\documentclass{<<.document>>}
        \\usepackage[margin=0pt]{geometry}
        \\usepackage{multicol}

        \\usepackage{titlesec}
        \\titleformat{name=\\section}
          {\\bf\\filcenter}{}{1em}{}{}

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
          state = .data$stateProvince,
          taxa = .data$scientificName,
          author = .data$scientificNameAuthorship,
          type = dplyr::if_else(
            condition = is.na(.data$typeStatus), true = "",
            false = glue::glue_data(.record, " \\hfill{{}} {typeStatus}")
          ),
          county = .data$county,
          latitude = .data$decimalLatitude,
          longitude = .data$decimalLongitude,
          locale = .data$verbatimLocality,
          # TODO Handle range of elevelations; unit conversion: m -> f
          elevation = .data$verbatimElevation,
          elev_unit = "ft. elev.",
          remarks = .data$occurrenceRemarks %|% "",
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
          <<habitat>>
          <<associatedTaxa>>

          <<remarks>>

          \\begin{center}
          \\begin{footnotesize}
          \\textbf{<<institutionName>>}
          \\end{footnotesize}
          \\end{center}

          <<closing>>
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
          \\bigskip
        "
      )
    }
  ),
  public = list(
    occurrences = NULL,
    initialize = function(records) {
      self$occurrences <- records |>
        dplyr::select(dplyr::all_of(private$terms)) |>
        # Many-to-one joins against internal package datasets: R/sysdata.R
        dplyr::left_join(y = taxa, by = "scientificName") |>
        dplyr::left_join(y = institutions, by = "institutionCode") |>
        dplyr::mutate(
          scientificNameAuthorship = gsub(
            pattern = "&",
            replacement = "\\&",
            x = scientificNameAuthorship,
            fixed = TRUE
          ),
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
      .NotYetImplemented()
    }
  )
)
