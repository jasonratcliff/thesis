#' @title Voucher Labels
#'
#' @description
#' Format occurrence data for `LaTeX` typesetting of voucher labels.
#' For each record, coordinates are annotated relative to a spatial reference
#' system axis (i.e., ± longitude and latitude), elevation data are
#' converted from m to ft. and `eventDates` are converted from `POSIXct` class
#' dates using [base::strptime()] specifications. Associated taxa split from `|`
#' separated values in `associatedTaxa`.
#'
#' @details
#' The following [Darwin Core](https://dwc.tdwg.org/terms/)
#' (DWC) terms are expected:
#' * Record-level:
#'   - `institutionCode`
#' * Occurrence:
#'   - `recordNumber`
#'   - `recordedBy`
#'   - `associatedTaxa`
#'   - `occurrenceRemarks`
#' * Event:
#'   - `eventDate`
#'   - `habitat`
#' * Location:
#'   - `stateProvince`
#'   - `county`
#'   - `verbatimLocality`
#'   - `decimalLatitude`
#'   - `decimalLongitude`
#'   - `minimumElevationInMeters`
#'   - `maximumElevationInMeters`
#' * Identification:
#'   - `typeStatus`
#' * Taxon:
#'   - `scientificName`
#'
#' @examples
#' fieldwork <- vouchers |>
#'   dplyr::filter(
#'     datasetID == "Fieldwork",
#'     grepl("eburniflora", scientificName)
#'   )
#' label <- Labels$new(records = fieldwork)
#'
#' label$tex
#'
#' @seealso [thesis::Taxa]
#' @include taxa.R
#' @export
Labels <- R6::R6Class(
  classname = "Labels",
  private = list(
    .occurrences = NULL,
    .taxa = NULL,
    .terms = {
      c(
        "scientificName", "institutionCode",
        "stateProvince", "county", "verbatimLocality",
        "recordedBy", "recordNumber", "typeStatus",
        "decimalLatitude", "decimalLongitude",
        "minimumElevationInMeters", "maximumElevationInMeters",
        "habitat", "associatedTaxa", "occurrenceRemarks",
        "recordedBy", "recordNumber", "eventDate"
      )
    },
    coordinates = function(x, y) {
      lon <- ifelse(x > 0, "E", "W")
      lat <- ifelse(y > 0, "N", "S")
      glue::glue("{abs(y)}&deg;{lat} {abs(x)}&deg;{lon}")
    },
    elevation_ft = function(m_min, m_max) {
      meters <- c(m_min, m_max)
      meters <- meters[!is.na(meters)]
      # NOTE: Resolution of scale conversion in data-raw/specimens/vouchers.R
      # > Loss of floating point precision due to round() call in `vouchers`
      feet <- format(round(meters * 3.281), big.mark = ",")
      if (length(feet) > 1) feet <- paste(feet, collapse = "-")
      feet <- paste(feet, "ft. elev.")
      return(feet)
    },
    event = \(x) format(as.Date(x), "%e %B %Y"),
    associations = function(x) {
      if (!is.na(x)) {
        obs <- unlist(strsplit(x, split = " | ", fixed = TRUE))
        obs <- paste0("*", obs, "*")
        n <- length(obs)
        combined <- paste(obs[1:n - 1], collapse = ", ")
        combined <- paste(combined, obs[n], sep = " and ")
        combined <- paste0("With ", combined, ".")
        return(combined)
      } else {
        return(NA_character_)
      }
    },
    open = function(i) {
      # Open `minipage` nested with 2 column `multicol` environment.
      ifelse(
        test = i %% 2, no = "",
        yes = paste(
          "\\begin{minipage}{\\linewidth}",
          "\\begin{multicols}{2}",
          sep = "\n"
        )
      )
    },
    close = function(i, n) {
      # Handle even-numbered or final odd-numbered record to close.
      ends <- list(
        odd = c("\\columnbreak", "\\vspace*{\\fill}"),
        env = c(
          "\\end{multicols}",
          "\\end{minipage}",
          "\\vspace{12pt}\n"
        )
      )
      count <- list(odd = (i %% 2), final = (i == n))
      if (count$odd) {
        if (count$final) {
          ending <- c(ends$odd, ends$env)
        } else {
          ending <- "\\columnbreak"
        }
      } else {
        ending <- ends$env
      }
      ending <- paste(ending, collapse = "\n")
      return(ending)
    },
    preamble = function(.document = "article") {
      .document <- match.arg(arg = .document, choices = "article")
      glue::glue_safe("
        %% LaTeX Preamble
        \\documentclass{<<.document>>}
        \\usepackage[margin=1em]{geometry}
        \\usepackage{multicol}
        \\raggedcolumns
        \\frenchspacing
        \\usepackage{titlesec}
        \\titleformat{name=\\section}
          {\\bf\\filcenter}{}{1em}{}{}
        \\setlength\\parindent{0pt}
        \\setlength{\\columnsep}{1em}
        \\begin{document}
        ",
        .sep = "\n", .open = "<<", .close = ">>"
      )
    },
    label = function(.record) {
      stopifnot(inherits(.record, "tbl_df") & nrow(.record) == 1)
      glue::glue_data_safe(
        .x = .record, "
          <<open>>
          \\section{Flora of <<stateProvince>>}
          <<taxon>> \\hfill{} \\textbf{<<typeStatus>>} \\newline
          \\newline
          <<county>> County: <<verbatimLocality>> \\newline
          <<coordinates>> \\hfill{} <<elevation_ft>> \\newline

          <<description>>

          <<remarks>>

          <<recordedBy>> <<recordNumber>> \\hfill{} <<eventDate>>\\newline
          {\\centering \\textbf{\\footnotesize{<<institutionName>>}} \\par}
          <<close>>
          ",
        .open = "<<", .close = ">>", .na = ""
      )
    }
  ),
  active = list(
    #' @field occurrences Collection record occurrence data
    occurrences = function() {
      private$.occurrences
    },
    #' @field tex `LaTeX`-formatted record labels
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
    }
  ),
  public = list(
    #' @description Initialize `Labels` class R6 object from specimen records.
    #' @param records Occurrence records [tbl-df] with expected DWC terms.
    initialize = function(records) {
      private$.taxa <- Taxa$new(thesis:::taxa)
      private$.occurrences <- records |>
        dplyr::select(dplyr::all_of(private$.terms)) |>
        dplyr::left_join(y = thesis:::codes, by = "institutionCode") |>
        dplyr::mutate(
          id = dplyr::row_number(),
          open = private$open(id),
          close = purrr::map_chr(
            .x = id, .f = \(x) private$close(x, n = nrow(records))
          ),
          taxon = private$.taxa$authorship(taxon = .data$scientificName),
          typeStatus = toupper(.data$typeStatus),
          coordinates = purrr::map2_chr(
            .x = .data$decimalLongitude,
            .y = .data$decimalLatitude,
            .f = \(x, y) private$coordinates(x, y)
          ),
          elevation_ft = purrr::map2_chr(
            .x = .data$minimumElevationInMeters,
            .y = .data$maximumElevationInMeters,
            .f = \(x, y) private$elevation_ft(x, y)
          ),
          eventDate = private$event(eventDate),
          associations = purrr::map_chr(
            .x = .data$associatedTaxa,
            .f = \(x) private$associations(x)
          ),
          description = glue::glue_data(
            .x = list("habitat" = habitat, "associations" = associations),
            "{habitat}", "{associations}",
            .na = "", .sep = " "
          ),
          dplyr::across(
            .cols = dplyr::all_of(c("institutionCode", "recordedBy")),
            .fns = \(x) gsub("&", "\\&", x = x, fixed = TRUE)
          ),
          dplyr::across(
            .cols = dplyr::all_of(c("taxon", "coordinates", "description")),
            .fns = \(x) purrr::map_chr(
              .x = x, .f = ~ commonmark::markdown_latex(.x)
            )
          ),
          description = ifelse(
            nzchar(description), paste(description, "\\newline"), ""
          ),
          remarks = ifelse(
            is.na(occurrenceRemarks), "",
            paste(occurrenceRemarks, "\\newline")
          )
        )
    },
    #' @description
    #' Render PDF-formatted voucher labels.
    #' @param tex Path to `.tex` file for rendering by [tinytex::pdflatex()].
    #' @examples
    #' \dontrun{
    #'   # Render `labels.pdf` from *.tex intermediate file
    #'   label$pdf(tex = tempfile("labels.tex"))
    #' }
    pdf = function(tex) {
      stopifnot(fs::path_ext(tex) == "tex")
      publish <- fs::path_dir(tex)
      if (!fs::dir_exists(publish)) {
        confirmed <-
          usethis::ui_yeah("Create label directory: {ui_path(publish)}")
        if (confirmed) {
          fs::dir_create(publish)
          usethis::ui_done("New directory: {ui_path(publish)}")
        }
      }
      flattened <- self$tex |>
        purrr::list_flatten() |>
        purrr::list_c()
      cat(flattened, file = tex, sep = "\n")
      tinytex::pdflatex(file = tex)
    }
  )
)
