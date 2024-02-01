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
        \\newenvironment{record}
            {
              \\begin{minipage}{\\linewidth}
              \\begin{tabular}{|p{\\linewidth}}
            }
            {
              \\end{tabular}
              \\end{minipage}
            }

        \\begin{document}
        \\begin{multicols}{2}
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
          .keep = "unused"
        )
      glue::glue_data_safe(
        .x = collection,
        "\\section{Flora of <<state>>}",
        "\\textit{\\bold{<<taxa>>}} <<author>><<type>>",
        "<<county>> County: <<locale>>",
        "<<latitude>>°N <<longitude>>°W <<elevation>> <<elev_unit>>",
        .open = "<<", .close = ">>", .sep = "\n\n"
      )
    }
  ),
  public = list(
    occurrences = NULL,
    initialize = function(records) {
      self$occurrences <- records |>
        dplyr::select(dplyr::all_of(private$terms))
    },
    tex = function() {
      # Map *p-variables* from *n-records* of specimen occurrences.
      labels <- purrr::pmap(
        .l = self$occurrences,
        .f = function(...) {
          l <- rlang::dots_list(..., .named = TRUE)
          private$label(.record = tibble::as_tibble(l))
        }
      )
      composed <-
        list(
          preamble = private$preamble(),
          labelled = labels,
          end = glue::glue(
            "\\end{{multicols}}",
            "\\end{{document}}",
            .sep = "\n"
          )
        )
      return(composed)
    },
    pdf = function(.write = FALSE) {
      if (.write) stop("Unimplemented")
    }
  )
)
