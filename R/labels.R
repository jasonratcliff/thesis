#' @title Voucher Labels
#'
#' Format occurrence data for `LaTeX` typesetting of voucher labels.
#' @include taxa.R
#' @export
Labels <- R6::R6Class(
  classname = "Labels",
  private = list(
    # Initialize R6 instance to join taxonomic terms
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
    event = function(x) { format(as.Date(x), "%d %B %Y") },
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
          taxon = private$.taxa$authorship(taxon = .data$scientificName) |>
            commonmark::markdown_latex(),
          type = dplyr::if_else(
            condition = is.na(.data$typeStatus), true = "",
            false = glue::glue_data(.record, " \\hfill{{}} {typeStatus}")
          ),
          remarks = .data$occurrenceRemarks %|% "",
          .keep = "unused"
        )

      glue::glue_data_safe(
        .x = collection, "
          <<open>>
          \\section{Flora of <<stateProvince>>}

          <<taxon>> <<type>>

          \\bigskip
          <<county>> County: <<verbatimLocality>>
          \\newline
          <<coordinates>> \\hfill{} <<elevation_ft>>

          <<description>>

          <<remarks>>

          <<recordedBy>> <<recordNumber>> \\hfill{} <<eventDate>>

          \\begin{center}
          \\begin{footnotesize}
          \\textbf{<<institutionName>>}
          \\end{footnotesize}
          \\end{center}

          <<close>>
          ",
        .open = "<<", .close = ">>",
        .sep = "\n"
      )
    }
  ),
  public = list(
    occurrences = NULL,
    initialize = function(records) {
      private$.taxa <- Taxa$new(thesis:::taxa)
      self$occurrences <- records |>
        dplyr::select(dplyr::all_of(private$.terms)) |>
        dplyr::left_join(y = thesis:::codes, by = "institutionCode") |>
        dplyr::mutate(
          id = dplyr::row_number(),
          open = private$open(id),
          close = purrr::map_chr(
            .x = id, .f = \(x) private$close(x, n = nrow(records))
          ),
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
            .cols = dplyr::all_of(c("coordinates")),
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
    #' @description
    #' Render PDF-formatted voucher labels.
    #' @param tex Path to `.tex` file for render by [tinytex::pdflatex()].
    pdf = function(tex = "docs/labels/label.tex") {
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
      flattened <- self$tex() |>
        purrr::list_flatten() |>
        purrr::list_c()
      cat(flattened, file = tex, sep = "\n")
      tinytex::pdflatex(file = tex)
    }
  )
)
